
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.Utils._
import firrtl.options.Dependency

import firrtl.transforms._
import scala.collection.mutable


case class ConditionalEdgeAnnotation(parentCondition: Target, childCondition: Target)
    extends MultiTargetAnnotation {

  override val targets: Seq[Seq[Target]] = Seq(Seq(parentCondition), Seq(childCondition))
  override def duplicate(n: Seq[Seq[Target]]): ConditionalEdgeAnnotation = this.copy(n(0).head, n(1).head)
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

  def onModule(m: DefModule, main: String): (DefModule, Seq[Annotation]) = {
    println(m.name)
    val annos = mutable.ListBuffer[Annotation]()
    m.map(x => onStmt(x, main, m.name, None, annos))
    val newAnnos = annos.toList
    (m, newAnnos)
  }

  def onStmt(s: Statement, main: String, m: String, parent: Option[ir.Expression], annos: mutable.ListBuffer[Annotation]): Statement = s match {
    case c: Conditionally =>
      println(s"Conditionally: ${c.pred}")
      parent match {
        case Some(p) => 
          val parentTarget = Utils.toTarget(main, m)(p)
          val childTarget = Utils.toTarget(main, m)(c.pred)
          val edgeAnno = ConditionalEdgeAnnotation(parentTarget, childTarget)
          val dontTouchParent = DontTouchAnnotation(parentTarget)
          val dontTouchChild = DontTouchAnnotation(childTarget)
          annos.append(edgeAnno)
          annos.append(dontTouchParent)
          annos.append(dontTouchChild)
        case None =>
      }
      println(s"edge: ${parent} --> ${c.pred}")
      onStmt(c.conseq, main, m, Some(c.pred), annos)
      onStmt(c.alt, main, m, Some(c.pred), annos)
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
