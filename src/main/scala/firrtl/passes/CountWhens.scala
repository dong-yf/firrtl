
package firrtl.passes

import firrtl._
import firrtl.ir._
import firrtl.annotations._
import firrtl.Mappers._
import firrtl.Utils._
import firrtl.options.Dependency
import java.util.concurrent.locks.Condition

object CountWhens extends Transform with DependencyAPIMigration {
  override def prerequisites =
    Seq(
      Dependency(PullMuxes),
      Dependency(ReplaceAccesses),
      Dependency(ExpandConnects),
      Dependency(RemoveAccesses)
    ) ++ firrtl.stage.Forms.Deduped

  override def invalidates(a: Transform) = false

  def onModule(m: DefModule): DefModule = {
    println(m.name)
    m.map(onStmt)
  }

  def onStmt(s: Statement): Statement = {
    s.map(onStmt) match {
      case c: Conditionally =>
        println(s"Conditionally: ${c.pred}")
        c
      case x => x
    }
  }

  def execute(state: CircuitState): CircuitState = {
    val circuit = state.circuit
    
    circuit.map(onModule(_))
  
    state
  }
}
