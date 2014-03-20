package joos.analysis.reachability

import joos.ast.statements.Block

class ReachableBlock(block: Block) extends Reachable {
  override def statement = block

  override def verify() {
    super.verify()

    if (block.statements.length == 0) {
      this.canFinish = this.canStart
      return
    }

    // canStart[S.0] = canStart[block]
    var lastStatement: Reachable = block.statements.head
    lastStatement.canStart = this.canStart
    lastStatement.verify()

    // canStart[S.i] = canFinish[S.i-1]
    for (i <- 1 until block.statements.length) {
      val statement: Reachable = block.statements(i)
      statement.canStart = lastStatement.canFinish
      statement.verify()
      lastStatement = statement
    }

    // canStart[block] = canFinish[S.last]
    this.canFinish = lastStatement.canFinish
  }
}
