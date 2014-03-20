package joos.analysis

import joos.ast.statements._
import scala.language.implicitConversions

package object reachability {

  private[this] class ReachableStatement(val statement: Statement) extends Reachable {
    override def verify() {
      super.verify()
      this.canFinish = this.canStart
    }
  }

  implicit def toReachable(statement: Statement): Reachable = {
    statement match {
      case statement: Block => new ReachableBlock(statement)
      case statement: IfStatement => new ReachableIf(statement)
      case statement: ReturnStatement => new ReachableReturn(statement)
      case statement: WhileStatement => new ReachableWhile(statement)
      case statement: ForStatement => new ReachableFor(statement)
      case _ => new ReachableStatement(statement)
    }
  }
}
