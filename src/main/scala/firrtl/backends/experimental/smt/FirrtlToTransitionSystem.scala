// SPDX-License-Identifier: Apache-2.0
// Author: Kevin Laeufer <laeufer@cs.berkeley.edu>

package firrtl.backends.experimental.smt

import firrtl.annotations.{MemoryInitAnnotation, NoTargetAnnotation, PresetRegAnnotation}
import firrtl._
import firrtl.backends.experimental.smt.random._
import firrtl.options.Dependency
import firrtl.passes.MemPortUtils.memPortField
import firrtl.passes.PassException
import firrtl.passes.memlib.VerilogMemDelays
import firrtl.passes.{PositiveEdgeAnnotation, NegativeEdgeAnnotation}
import firrtl.stage.Forms
import firrtl.stage.TransformManager.TransformDependency
import firrtl.transforms.{EnsureNamedStatements, PropagatePresetAnnotations}
import logger.LazyLogging

import scala.collection.mutable
import scala.util.matching.Regex

case class TransitionSystemAnnotation(sys: TransitionSystem) extends NoTargetAnnotation

/** Contains code to convert a flat firrtl module into a functional transition system which
  * can then be exported as SMTLib or Btor2 file.
  */
object FirrtlToTransitionSystem extends Transform with DependencyAPIMigration {
  override def prerequisites: Seq[Dependency[Transform]] = Forms.LowForm ++
    Seq(
      Dependency(VerilogMemDelays),
      Dependency(EnsureNamedStatements), // this is required to give assert/assume statements good names
      Dependency[PropagatePresetAnnotations]
    )
  override def invalidates(a: Transform): Boolean = false
  // since this pass only runs on the main module, inlining needs to happen before
  override def optionalPrerequisites: Seq[TransformDependency] = Seq(Dependency[firrtl.passes.InlineInstances])

  override protected def execute(state: CircuitState): CircuitState = {
    // println("-------enter execute---------")
    val circuit = state.circuit
    val presetRegs = state.annotations.collect {
      case PresetRegAnnotation(target) if target.module == circuit.main => target.ref
    }.toSet

    // get suffix map form annotation
    // FIXME: a child may have multiple parents
    val decisionVarsMap = mutable.Map[String, String]()
    state.annotations.collect {
      case PositiveEdgeAnnotation(parentCondition, childCondition, pIndex, cIndex) => 
        val pattern = """.*?>(.*)""".r
        val matchres = pattern.findFirstMatchIn(childCondition.serialize).map(_.group(1))
        val childName = matchres.get
        val suffix = s"${childName}_g#${cIndex}:${cIndex}"
        // println(s"${suffix}")
        // println(s"[PositiveEdge]: ${childCondition.serialize}")
        childName -> suffix
        decisionVarsMap += (childName -> suffix)
      case NegativeEdgeAnnotation(parentCondition, childCondition, pIndex, cIndex) => 
        val pattern = """.*?>(.*)""".r
        val matchres = pattern.findFirstMatchIn(childCondition.serialize).map(_.group(1))
        val childName = matchres.get
        val suffix = s"${childName}_g#${cIndex}:${cIndex}"
        // println(s"${suffix}")
        // println(s"[NegativeEdge]: ${childCondition.serialize}")
        childName -> suffix
        decisionVarsMap += (childName -> suffix)
    }

    state.annotations.collect {
      case PositiveEdgeAnnotation(parentCondition, _, pIndex, _) => 
        val pattern = """.*?>(.*)""".r
        val matchres = pattern.findFirstMatchIn(parentCondition.serialize).map(_.group(1))
        val parentName = matchres.get
        val suffix = s"${parentName}_g#${pIndex}:${pIndex}"
        parentName -> suffix
      case NegativeEdgeAnnotation(parentCondition, _, pIndex, _) => 
        val pattern = """.*?>(.*)""".r
        val matchres = pattern.findFirstMatchIn(parentCondition.serialize).map(_.group(1))
        val parentName = matchres.get
        val suffix = s"${parentName}_g#${pIndex}:${pIndex}"
        parentName -> suffix
    }.foreach { case (parentName, suffix) =>
      if (!decisionVarsMap.contains(parentName)) {
        // println(s"[DecisionVarsMap]: ${parentName} ${suffix}")
        decisionVarsMap += (parentName -> suffix)
      }
    }

    // collect all non-random memory initialization
    val memInit = state.annotations.collect { case a: MemoryInitAnnotation if !a.isRandomInit => a }
      .filter(_.target.module == circuit.main)
      .map(a => a.target.ref -> a.initValue)
      .toMap

    // module look up table
    val modules = circuit.modules.map(m => m.name -> m).toMap

    // collect uninterpreted module annotations
    val uninterpreted = state.annotations.collect {
      case a: UninterpretedModuleAnnotation =>
        UninterpretedModuleAnnotation.checkModule(modules(a.target.module), a)
        a.target.module -> a
    }.toMap

    // convert the main module
    val main = modules(circuit.main)
    val sys = main match {
      case _: ir.ExtModule =>
        throw new ExtModuleException(
          "External modules are not supported by the SMT backend. Use yosys if you need to convert Verilog."
        )
      case m: ir.Module =>
        new ModuleToTransitionSystem(presetRegs = presetRegs, memInit = memInit, uninterpreted = uninterpreted).run(m)
    }

    val sortedSys = TopologicalSort.run(sys)

    // update state and signal names
    val updatedStates = sortedSys.states.map {
      case state @ State(sym, init, next) =>
        // println(s"[State]: ${sym.name} ${init} ${next}")
        val newsym = sym.rename(decisionVarsMap.getOrElse(sym.name, sym.name))
        val newinit = if (!init.isEmpty) init.map(expr => suffixEncoder.updateStatesAndSignals(expr, decisionVarsMap)) else None
        val newnext = if (!next.isEmpty) next.map(expr => suffixEncoder.updateStatesAndSignals(expr, decisionVarsMap)) else None
        // println(s"[Updated State]: ${newsym.name} ${newinit} ${newnext}")
        State(newsym, newinit, newnext)
    }

    val updatedInputs = sortedSys.inputs.map {
      case input @ BVSymbol(name, width) =>
        val updatedName = decisionVarsMap.getOrElse(name, name)
        // println(s"[Input]: ${name} ${width}")
        // println(s"[Updated Input]: ${updatedName} ${width}")
        BVSymbol(updatedName, width)
    }

    val updatedSignals = sortedSys.signals.map {
    case signal @ Signal(name, expr, kind) =>
      // println(s"[Signal]: ${name} ${expr} ${kind}")
      val updatedName = decisionVarsMap.getOrElse(name, name)
      val updatedExpr = suffixEncoder.updateStatesAndSignals(expr, decisionVarsMap)
      // println(s"[Updated Signal]: ${updatedName} ${updatedExpr} ${kind}")
      Signal(updatedName, updatedExpr, kind)
    }
    val updatedSys = sortedSys.copy(states = updatedStates, inputs = updatedInputs, signals = updatedSignals)

    val anno = TransitionSystemAnnotation(updatedSys)
    state.copy(circuit = circuit, annotations = state.annotations :+ anno)
  }
}

object suffixEncoder{
  def updateStatesAndSignals(expr: SMTExpr, decisionVarsMap: mutable.Map[String, String]): SMTExpr = {
    expr match {
      case sym: SMTSymbol => 
        val updatedName = decisionVarsMap.getOrElse(sym.name, sym.name)
        sym.rename(updatedName)
      case other => SMTExprMap.mapExpr(other, updateStatesAndSignals(_, decisionVarsMap))
    }
  }

  def updateSignalExpr(expr: SMTExpr, decisionVarsMap: mutable.Map[String, String]): SMTExpr = {
    expr match {
      case bvs: BVSymbol => 
        val updatedName = decisionVarsMap.getOrElse(bvs.name, bvs.name)
        BVSymbol(updatedName, bvs.width)
      case arrs: ArraySymbol => 
        val updatedName = decisionVarsMap.getOrElse(arrs.name, arrs.name)
        ArraySymbol(updatedName, arrs.indexWidth, arrs.dataWidth)
      case bvfc: BVFunctionCall =>
        // TODO: deal with BVFunctionCall
        println(s"[BVFunctionCall]: ${bvfc.name} ${bvfc.args} ${bvfc.width}")
        bvfc
      case arrfc: ArrayFunctionCall =>
        println(s"[ArrayFunctionCall]: ${arrfc.name} ${arrfc.args} ${arrfc.indexWidth} ${arrfc.dataWidth}")
        arrfc
      case bvUnaryExpr: BVUnaryExpr =>
        println(s"[BVUnaryExpr]: ${bvUnaryExpr.e}")
        updateBVSignal(bvUnaryExpr, decisionVarsMap)
      case bvBinaryExpr: BVBinaryExpr =>
        println(s"[BVBinaryExpr]: ${bvBinaryExpr.a} ${bvBinaryExpr.b}")
        updateBVSignal(bvBinaryExpr, decisionVarsMap)
      case bvor @ BVOr(terms) =>
        println(s"[BVOr]: ${bvor.terms}")
        val updatedTerms = terms.map(term => updateBVSignal(term, decisionVarsMap))
        BVOr(updatedTerms)
      case ite @ BVIte(cond, tru, fals) =>
        println(s"[BVIte]: ${ite.cond} ${ite.tru} ${ite.fals}")
        val updatedCond = updateBVSignal(cond, decisionVarsMap)
        val updatedTru = updateBVSignal(tru, decisionVarsMap)
        val updatedFals = updateBVSignal(fals, decisionVarsMap)
        BVIte(updatedCond, updatedTru, updatedFals)
      case _ => expr
    }
  }

  def updateBVSignal(expr: BVExpr, decisionVarsMap: mutable.Map[String, String]): BVExpr = {
    expr match {
      case bvs: BVSymbol => 
        val updatedName = decisionVarsMap.getOrElse(bvs.name, bvs.name)
        BVSymbol(updatedName, bvs.width)
      case bvUnaryExpr: BVUnaryExpr =>
        println(s"[BVUnaryExpr]: ${bvUnaryExpr.e}")
        bvUnaryExpr.reapply(updateBVSignal(bvUnaryExpr.e, decisionVarsMap))
      case bvBinaryExpr: BVBinaryExpr =>
        println(s"[BVBinaryExpr]: ${bvBinaryExpr.a} ${bvBinaryExpr.b}")
        bvBinaryExpr.reapply(updateBVSignal(bvBinaryExpr.a, decisionVarsMap),
          updateBVSignal(bvBinaryExpr.b, decisionVarsMap))
      case bvand @ BVAnd(terms) =>
        println(s"[BVAnd]: ${bvand.terms}")
        val updatedTerms = terms.map(term => updateBVSignal(term, decisionVarsMap))
        BVAnd(updatedTerms)
      case _ => expr
    }
  }
}

private object UnsupportedException {
  val HowToRunStuttering: String =
    """
      |You can run the StutteringClockTransform which
      |replaces all clock inputs with a clock enable signal.
      |This is required not only for multi-clock designs, but also to
      |accurately model asynchronous reset which could happen even if there
      |isn't a clock edge.
      | If you are using the firrtl CLI, please add:
      |   -fct firrtl.backends.experimental.smt.StutteringClockTransform
      | If you are calling into firrtl programmatically you can use:
      |   RunFirrtlTransformAnnotation(Dependency[StutteringClockTransform])
      | To designate a clock to be the global_clock (i.e. the simulation tick), use:
      |   GlobalClockAnnotation(CircuitTarget(...).module(...).ref("your_clock")))
      |""".stripMargin
}

private class ExtModuleException(s: String) extends PassException(s)
private class AsyncResetException(s: String) extends PassException(s + UnsupportedException.HowToRunStuttering)
private class MultiClockException(s: String) extends PassException(s + UnsupportedException.HowToRunStuttering)
private class MissingFeatureException(s: String)
    extends PassException("Unfortunately the SMT backend does not yet support: " + s)

private class ModuleToTransitionSystem(
  presetRegs:    Set[String],
  memInit:       Map[String, MemoryInitValue],
  uninterpreted: Map[String, UninterpretedModuleAnnotation])
    extends LazyLogging {

  def run(m: ir.Module): TransitionSystem = {
    // first pass over the module to convert expressions; discover state and I/O
    m.foreachPort(onPort)
    m.foreachStmt(onStatement)

    // multi-clock support requires the StutteringClock transform to be run
    if (clocks.size > 1) {
      throw new MultiClockException(s"The module ${m.name} has more than one clock: ${clocks.mkString(", ")}")
    }

    // generate comments from infos
    val comments = mutable.HashMap[String, String]()
    infos.foreach {
      case (name, info) =>
        val infoStr = info.serialize.trim
        if (infoStr.nonEmpty) {
          val prefix = comments.get(name).map(_ + ", ").getOrElse("")
          comments(name) = prefix + infoStr
        }
    }

    // module info to the comment header
    val header = m.info.serialize.trim

    TransitionSystem(m.name, inputs.toList, states.values.toList, signals.toList, comments.toMap, header)
  }

  private val inputs = mutable.ArrayBuffer[BVSymbol]()
  private val clocks = mutable.ArrayBuffer[String]()
  private val signals = mutable.ArrayBuffer[Signal]()
  private val states = mutable.LinkedHashMap[String, State]()
  private val infos = mutable.ArrayBuffer[(String, ir.Info)]()

  private def onPort(p: ir.Port): Unit = {
    if (isAsyncReset(p.tpe)) {
      throw new AsyncResetException(s"Found AsyncReset ${p.name}.")
    }
    infos.append(p.name -> p.info)
    p.direction match {
      case ir.Input =>
        if (isClock(p.tpe)) {
          clocks.append(p.name)
        } else {
          inputs.append(BVSymbol(p.name, bitWidth(p.tpe).toInt))
        }
      case ir.Output =>
    }
  }

  private def onStatement(s: ir.Statement): Unit = s match {
    case DefRandom(info, name, tpe, _, en) =>
      assert(!isClock(tpe), "rand should never be a clock!")
      // we model random sources as inputs and the enable signal as output
      infos.append(name -> info)
      inputs.append(BVSymbol(name, bitWidth(tpe).toInt))
      signals.append(Signal(name + ".en", onExpression(en, 1), IsOutput))
    case w: ir.DefWire =>
      if (!isClock(w.tpe)) {
        // InlineInstances can insert wires without re-running RemoveWires for now we just deal with it when
        // the Wires is connected to (ir.Connect).
      }
    case ir.DefNode(info, name, expr) =>
      if (!isClock(expr.tpe) && !isAsyncReset(expr.tpe)) {
        infos.append(name -> info)
        // println("Signal: " + name + " " + expr.serialize)
        signals.append(Signal(name, onExpression(expr), IsNode))
      }
    case r: ir.DefRegister =>
      infos.append(r.name -> r.info)
      states(r.name) = onRegister(r)
    case m: ir.DefMemory =>
      infos.append(m.name -> m.info)
      states(m.name) = onMemory(m)
    case ir.Connect(info, loc, expr) =>
      if (!isGroundType(loc.tpe)) error("All connects should have been lowered to ground type!")
      if (!isClock(loc.tpe) && !isAsyncReset(expr.tpe)) { // we ignore clock connections
        val name = loc.serialize
        val e = onExpression(expr, bitWidth(loc.tpe).toInt, allowNarrow = false)
        Utils.kind(loc) match {
          case RegKind => states(name) = states(name).copy(next = Some(e))
          case PortKind | InstanceKind => // module output or submodule input
            infos.append(name -> info)
            signals.append(Signal(name, e, IsOutput))
          case MemKind | WireKind =>
            // InlineInstances can insert wires without re-running RemoveWires for now we just deal with it.
            infos.append(name -> info)
            signals.append(Signal(name, e, IsNode))
        }
      }
    case i: ir.IsInvalid =>
      throw new UnsupportedFeatureException(s"IsInvalid statements are not supported: ${i.serialize}")
    case ir.DefInstance(info, name, module, tpe) => onInstance(info, name, module, tpe)
    case s: ir.Verification =>
      if (s.op == ir.Formal.Cover) {
        logger.info(s"[info] Cover statement was ignored: ${s.serialize}")
      } else {
        val name = s.name
        val predicate = onExpression(s.pred)
        val enabled = onExpression(s.en)
        val e = BVImplies(enabled, predicate)
        infos.append(name -> s.info)
        val signal = if (s.op == ir.Formal.Assert) {
          Signal(name, BVNot(e), IsBad)
        } else {
          Signal(name, e, IsConstraint)
        }
        signals.append(signal)
      }
    case s: ir.Conditionally =>
      error(s"When conditions are not supported. Please run ExpandWhens: ${s.serialize}")
    case s: ir.PartialConnect =>
      error(s"PartialConnects are not supported. Please run ExpandConnects: ${s.serialize}")
    case s: ir.Attach =>
      error(s"Analog wires are not supported in the SMT backend: ${s.serialize}")
    case s: ir.Stop =>
      if (s.ret == 0) {
        logger.info(
          s"[info] Stop statements with a return code of 0 are currently not supported. Ignoring: ${s.serialize}"
        )
      } else {
        // we treat Stop statements with a non-zero exit value as assertions that en will always be false!
        val name = s.name
        infos.append(name -> s.info)
        signals.append(Signal(name, onExpression(s.en), IsBad))
      }
    case s: ir.Print =>
      logger.info(s"Info: ignoring: ${s.serialize}")
    case other => other.foreachStmt(onStatement)
  }

  private def onRegister(r: ir.DefRegister): State = {
    val width = bitWidth(r.tpe).toInt
    val resetExpr = onExpression(r.reset, 1)
    assert(resetExpr == False(), s"Expected reset expression of ${r.name} to be 0, not $resetExpr")
    val initExpr = onExpression(r.init, width)
    val sym = BVSymbol(r.name, width)
    val hasReset = initExpr != sym
    val isPreset = presetRegs.contains(r.name)
    assert(!isPreset || hasReset, s"Expected preset register ${r.name} to have a reset value, not just $initExpr!")
    val state = State(sym, if (isPreset) Some(initExpr) else None, None)
    state
  }

  private def onInstance(info: ir.Info, name: String, module: String, tpe: ir.Type): Unit = {
    if (!tpe.isInstanceOf[ir.BundleType]) error(s"Instance $name of $module has an invalid type: ${tpe.serialize}")
    if (uninterpreted.contains(module)) {
      onUninterpretedInstance(info: ir.Info, name: String, module: String, tpe: ir.Type)
    } else {
      // We treat all instances that aren't annotated as uninterpreted as blackboxes
      // this means that their outputs could be any value, no matter what their inputs are.
      logger.warn(
        s"WARN: treating instance $name of $module as blackbox. " +
          "Please flatten your hierarchy if you want to include submodules in the formal model."
      )
      val ports = tpe.asInstanceOf[ir.BundleType].fields
      // skip async reset ports
      ports.filterNot(p => isAsyncReset(p.tpe)).foreach { p =>
        if (!p.tpe.isInstanceOf[ir.GroundType]) error(s"Instance $name of $module has an invalid port type: $p")
        val isOutput = p.flip == ir.Default
        val pName = name + "." + p.name
        infos.append(pName -> info)
        // outputs of the submodule become inputs to our module
        if (isOutput) {
          if (isClock(p.tpe)) {
            clocks.append(pName)
          } else {
            inputs.append(BVSymbol(pName, bitWidth(p.tpe).toInt))
          }
        }
      }
    }
  }

  private def onUninterpretedInstance(info: ir.Info, instanceName: String, module: String, tpe: ir.Type): Unit = {
    val anno = uninterpreted(module)

    // sanity checks for ports were done already using the UninterpretedModule.checkModule function
    val ports = tpe.asInstanceOf[ir.BundleType].fields

    val outputs = ports.filter(_.flip == ir.Default).map(p => BVSymbol(p.name, bitWidth(p.tpe).toInt))
    val inputs = ports.filterNot(_.flip == ir.Default).map(p => BVSymbol(p.name, bitWidth(p.tpe).toInt))

    assert(anno.stateBits == 0, "TODO: implement support for uninterpreted stateful modules!")

    // for state-less (i.e. combinatorial) circuits, the outputs only depend on the inputs
    val args = inputs.map(i => BVSymbol(instanceName + "." + i.name, i.width)).toList
    outputs.foreach { out =>
      val functionName = anno.prefix + "." + out.name
      val call = BVFunctionCall(functionName, args, out.width)
      val wireName = instanceName + "." + out.name
      signals.append(Signal(wireName, call))
    }
  }

  private def onMemory(m: ir.DefMemory): State = {
    checkMem(m)

    // derive the type of the memory from the dataType and depth
    val dataWidth = bitWidth(m.dataType).toInt
    val indexWidth = Utils.getUIntWidth(m.depth - 1).max(1)
    val memSymbol = ArraySymbol(m.name, indexWidth, dataWidth)

    // there could be a constant init
    val init = memInit.get(m.name).map(getMemInit(m, indexWidth, dataWidth, _))
    init.foreach(e => assert(e.dataWidth == memSymbol.dataWidth && e.indexWidth == memSymbol.indexWidth))

    // derive next state expression
    val next = if (m.writers.isEmpty) {
      memSymbol
    } else {
      m.writers.foldLeft[ArrayExpr](memSymbol) {
        case (prev, write) =>
          // update
          val addr = BVSymbol(memPortField(m, write, "addr").serialize, indexWidth)
          val data = BVSymbol(memPortField(m, write, "data").serialize, dataWidth)
          val update = ArrayStore(prev, index = addr, data = data)

          // update guard
          val en = BVSymbol(memPortField(m, write, "en").serialize, 1)
          val mask = BVSymbol(memPortField(m, write, "mask").serialize, 1)
          ArrayIte(BVAnd(en, mask), update, prev)
      }
    }

    val state = State(memSymbol, init, Some(next))

    // derive read expressions
    val readSignals = m.readers.map { read =>
      val addr = BVSymbol(memPortField(m, read, "addr").serialize, indexWidth)
      Signal(memPortField(m, read, "data").serialize, ArrayRead(memSymbol, addr), IsNode)
    }
    signals ++= readSignals

    state
  }

  private def getMemInit(m: ir.DefMemory, indexWidth: Int, dataWidth: Int, initValue: MemoryInitValue): ArrayExpr =
    initValue match {
      case MemoryScalarInit(value) => ArrayConstant(BVLiteral(value, dataWidth), indexWidth)
      case MemoryArrayInit(values) =>
        assert(
          values.length == m.depth,
          s"Memory ${m.name} of depth ${m.depth} cannot be initialized with an array of length ${values.length}!"
        )
        // in order to get a more compact encoding try to find the most common values
        val histogram = mutable.LinkedHashMap[BigInt, Int]()
        values.foreach(v => histogram(v) = 1 + histogram.getOrElse(v, 0))
        val baseValue = histogram.maxBy(_._2)._1
        val base = ArrayConstant(BVLiteral(baseValue, dataWidth), indexWidth)
        values.zipWithIndex
          .filterNot(_._1 == baseValue)
          .foldLeft[ArrayExpr](base) {
            case (array, (value, index)) =>
              ArrayStore(array, BVLiteral(index, indexWidth), BVLiteral(value, dataWidth))
          }
      case other => throw new RuntimeException(s"Unsupported memory init option: $other")
    }

  private def checkMem(m: ir.DefMemory): Unit = {
    assert(m.readLatency == 0, "Expected read latency to be 0. Did you run VerilogMemDelays?")
    assert(m.writeLatency == 1, "Expected read latency to be 1. Did you run VerilogMemDelays?")
    assert(
      m.dataType.isInstanceOf[ir.GroundType],
      s"Memory $m is of type ${m.dataType} which is not a ground type!"
    )
    assert(m.readwriters.isEmpty, "Combined read/write ports are not supported! Please split them up.")
  }

  private def onExpression(e: ir.Expression, width: Int, allowNarrow: Boolean = false): BVExpr =
    FirrtlExpressionSemantics.toSMT(e, width, allowNarrow)
  private def onExpression(e: ir.Expression): BVExpr = FirrtlExpressionSemantics.toSMT(e)

  private def error(msg:        String):  Unit = throw new RuntimeException(msg)
  private def isGroundType(tpe: ir.Type): Boolean = tpe.isInstanceOf[ir.GroundType]
  private def isClock(tpe:      ir.Type): Boolean = tpe == ir.ClockType
  private def isAsyncReset(tpe: ir.Type): Boolean = tpe == ir.AsyncResetType
}
