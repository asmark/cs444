package joos

import joos.parsetree.{ParseTree, TreeNode, ParseTreeNode, LeafNode}
import scala.collection.mutable
import joos.tokens.{TokenKind, Token}

class ParseTreeBuilder(actionTable: LrOneActionTable) {

  private final val BEGIN = "bof"
  private final val END = "eof"

  def build(tokens: Seq[Token]): ParseTree = {

    val nodeStack = mutable.Stack(LeafNode(BEGIN)): mutable.Stack[ParseTreeNode]
    val stateStack = mutable.Stack(actionTable.shift(0, BEGIN))

    // TODO: Terminals should be normalized into their token representations
    // val terminals = tokens.map(TokenKindValue.KindToSymbol)
    val terminals = tokens.map(token => TokenKind.kindToSymbol(token.kind))
    (terminals ++ Seq(END)).foreach {
      terminal =>
      // Reduce tokens while you are able to, looking ahead by one terminal [LR(1)]
        while (actionTable.isReduce(stateStack.top, terminal)) {
          val productionRule = actionTable.reduce(stateStack.top, terminal)

          val childNodes = Range(0, productionRule.derivation.length).map {
            i => stateStack.pop(); nodeStack.pop()
          }.reverse

          nodeStack.push(TreeNode(productionRule.base, childNodes))
          stateStack.push(actionTable.shift(stateStack.top, productionRule.base))
        }

        nodeStack.push(LeafNode(terminal))
        stateStack.push(actionTable.shift(stateStack.top, terminal))
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
