package joos.analysis.reachability

import joos.ast.statements.ReturnStatement
import joos.core.TernaryBoolean._

class ReachableReturn(val statement: ReturnStatement) extends Reachable {
  override def verify() {
    super.verify()
    this.canFinish = False
  }
}
