package joos.analysis.reachability

import joos.ast.statements.IfStatement

class ReachableIf(val statement: IfStatement) extends Reachable {
  override def verify() {
    super.verify()

    // canStart[true] = canStart[statement]
    val trueStatement: Reachable = statement.trueStatement
    trueStatement.canStart = this.canStart
    trueStatement.verify()

    // canStart[false] = canStart[statement]
    if (statement.falseStatement.isDefined) {
      val falseStatement: Reachable = statement.falseStatement.get
      falseStatement.canStart = this.canStart
      falseStatement.verify()

      // canFinish[statement] = canFinish[false] OR canFinish[true]
      this.canFinish = trueStatement.canFinish | falseStatement.canFinish
    } else {
      // canFinish[statement] = canStart[statement]
      this.canFinish = this.canStart
    }
  }
}
