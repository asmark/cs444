package joos

import joos.parsetree.{ParseTree, TreeNode, ParseTreeNode, LeafNode}
import scala.collection.mutable

class ParseTreeBuilder(actionTable: LrOneActionTable) {

  private final val BEGIN = "BOF"
  private final val END = "EOF"

  def build(terminals: Seq[String]): ParseTree = {

    val nodeStack = mutable.Stack(LeafNode(BEGIN)): mutable.Stack[ParseTreeNode]
    val stateStack = mutable.Stack(actionTable.shift(0, BEGIN))

    (terminals ++ Seq(END)).foreach {
      terminal =>
      // Reduce terminals while you are able to, looking ahead by one terminal [LR(1)]
        while (actionTable.isReduce(stateStack.top, terminal)) {
          val productionRule = actionTable.reduce(stateStack.top, terminal)

          val childNodes = Range(0, productionRule.derivation.length).map {
            i => stateStack.pop(); nodeStack.pop()
          }.toVector.reverse

          nodeStack.push(TreeNode(productionRule.base, childNodes))
          stateStack.push(actionTable.shift(stateStack.top, productionRule.base))
        }

        nodeStack.push(LeafNode(terminal))
        stateStack.push(actionTable.shift(stateStack.top, terminal))
    }

    assert(nodeStack.length == 3)
    ParseTree(TreeNode("root", nodeStack.toVector.reverse))
  }
}

object ParseTreeBuilder {
  def apply(actionTable : LrOneActionTable) : ParseTreeBuilder = {
    new ParseTreeBuilder(actionTable)
  }
}
