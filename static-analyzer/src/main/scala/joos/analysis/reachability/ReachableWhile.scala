package joos.analysis.reachability

import joos.ast.statements.WhileStatement

class ReachableWhile(val statement: WhileStatement) extends ReachableLoop {
  override def verify() {
    super.verify()
    verify(statement.condition, statement.body)
  }
}
