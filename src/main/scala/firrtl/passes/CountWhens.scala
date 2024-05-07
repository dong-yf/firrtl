
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.Utils._
import firrtl.options.Dependency

import firrtl.transforms._
import scala.collection.mutable


case class PositiveEdgeAnnotation(parentCondition: Target, childCondition: Target, pIndex: Int, cIndex: Int)
    extends MultiTargetAnnotation {
  
  override val targets: Seq[Seq[Target]] = Seq(Seq(parentCondition), Seq(childCondition))
  override def duplicate(n: Seq[Seq[Target]]): PositiveEdgeAnnotation = this.copy(n(0).head, n(1).head, pIndex, cIndex)
}

case class NegativeEdgeAnnotation(parentCondition: Target, childCondition: Target, pIndex: Int, cIndex: Int)
    extends MultiTargetAnnotation {

  override val targets: Seq[Seq[Target]] = Seq(Seq(parentCondition), Seq(childCondition))
  override def duplicate(n: Seq[Seq[Target]]): NegativeEdgeAnnotation = this.copy(n(0).head, n(1).head, pIndex, cIndex)
}


object CountWhens extends Transform with DependencyAPIMigration {
  override def prerequisites =
    Seq(
      Dependency(PullMuxes),
      Dependency(ReplaceAccesses),
      Dependency(ExpandConnects),
      Dependency(RemoveAccesses)
    ) ++ firrtl.stage.Forms.Deduped

  override def invalidates(a: Transform) = false

  // identify each condition with a index, according to the order of the condition
  // FIXME: 
  // 1. index should be reset for each module
  // 2. index may not be unique for each condition
  private var index = 1
  private val conditionIndexMap = mutable.HashMap[ir.Expression, Int]()

  def onModule(m: DefModule, main: String): (DefModule, Seq[Annotation]) = {
    println(m.name)
    val annos = mutable.ListBuffer[Annotation]()
    m.map(x => onStmt(x, main, m.name, None, annos))
    val newAnnos = annos.toList
    (m, newAnnos)
  }

  def onStmt(s: Statement, main: String, m: String, parent: Option[(ir.Expression, Boolean)], annos: mutable.ListBuffer[Annotation]): Statement = s match {
    case c: Conditionally =>
      println(s"Conditionally: ${c.pred}")
      parent match {
        case Some((p, b)) => 
          try {
            val parentTarget = Utils.toTarget(main, m)(p)
            val childTarget = Utils.toTarget(main, m)(c.pred)
            val pIndex = conditionIndexMap.getOrElse(p, -1)
            val cIndex = index
            val edgeAnno = if (b) PositiveEdgeAnnotation(parentTarget, childTarget, pIndex, cIndex) else NegativeEdgeAnnotation(parentTarget, childTarget, pIndex, cIndex)
            val dontTouchParent = DontTouchAnnotation(parentTarget)
            val dontTouchChild = DontTouchAnnotation(childTarget)
            annos.append(edgeAnno)
            annos.append(dontTouchParent)
            annos.append(dontTouchChild)
          } catch {
            case e: Exception => println("catch exception")
          }
        case None =>
          try {
            val parentTarget = Utils.toTarget(main, m)(c.pred)
            val childTarget = Utils.toTarget(main, m)(c.pred)
            val pIndex = index
            val cIndex = index
            val edgeAnno = PositiveEdgeAnnotation(parentTarget, childTarget, pIndex, cIndex)
            val dontTouchChild = DontTouchAnnotation(childTarget)
            annos.append(edgeAnno)
            annos.append(dontTouchChild)
          } catch {
            case e: Exception => println("catch exception")
          }
      }
      conditionIndexMap(c.pred) = index
      println(s"index: ${index}")
      index += 1
      println(s"edge: ${parent} --> ${c.pred}")
      onStmt(c.conseq, main, m, Some(c.pred, true), annos)
      onStmt(c.alt, main, m, Some(c.pred, false), annos)
      c
    case x => x.map(onStmt(_, main, m, parent, annos))
  }

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    
    val modsAndAnnos = circuit.modules.map(onModule(_, circuit.main))
    val newAnnos = modsAndAnnos.flatMap(_._2)
  
    state.copy(annotations = newAnnos ++: state.annotations)
  }
}
