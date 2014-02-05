package joos

import joos.parsetree.{ParseTree, TreeNode, ParseTreeNode, LeafNode}
import scala.collection.mutable
import joos.tokens.{TokenKind, Token}

class ParseTreeBuilder(actionTable: LrOneActionTable) {

  private final val BEGIN = "BOF"
  private final val END = "EOF"

  def build(tokens: Seq[Token]): ParseTree = {

    val nodeStack = mutable.Stack(LeafNode(BEGIN)): mutable.Stack[ParseTreeNode]
    val stateStack = mutable.Stack(actionTable.shift(0, BEGIN))

    val terminals = tokens.map(_.kind.toString)
    (terminals ++ Seq(END)).foreach {
      token =>
      // Reduce tokens while you are able to, looking ahead by one token [LR(1)]
        while (actionTable.isReduce(stateStack.top, token)) {
          val productionRule = actionTable.reduce(stateStack.top, token)

          val childNodes = Range(0, productionRule.derivation.length).map {
            i => stateStack.pop(); nodeStack.pop()
          }.reverse

          nodeStack.push(TreeNode(productionRule.base, childNodes))
          stateStack.push(actionTable.shift(stateStack.top, productionRule.base))
        }

        nodeStack.push(LeafNode(token))
        stateStack.push(actionTable.shift(stateStack.top, token))
    }

    assert(nodeStack.length == 3)
    ParseTree(nodeStack(1))
  }
}

object ParseTreeBuilder {
  def apply(actionTable: LrOneActionTable): ParseTreeBuilder = {
    new ParseTreeBuilder(actionTable)
  }
}
