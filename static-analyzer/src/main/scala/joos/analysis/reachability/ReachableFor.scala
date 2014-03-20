package joos.analysis.reachability

import joos.ast.statements.ForStatement
import joos.core.TernaryBoolean._

class ReachableFor(val statement: ForStatement) extends ReachableLoop {
  override def verify() {
    super.verify()

    val body: Reachable = statement.body
    statement.condition match {
      case None =>
        // canStart[body] = canStart[statement]
        // canFinish[statement] = false
        body.canStart = this.canStart
        body.verify()
        this.canFinish = False
      case Some(condition) => verify(condition, body)
    }
  }
}
