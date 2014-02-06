package joos

import joos.exceptions.JoosParseException
import joos.parsetree.{ParseTree, TreeNode, ParseTreeNode, LeafNode}
import joos.tokens.{TokenKind, Token}
import scala.collection.mutable

class ParseTreeBuilder(actionTable: LrOneActionTable) {

  private final val Begin = "bof"
  private final val End = "eof"
  private final val WindowSize = 10

  def build(tokens: Seq[Token]): ParseTree = {

    val nodeStack = mutable.Stack(LeafNode(Begin)): mutable.Stack[ParseTreeNode]
    val stateStack = mutable.Stack(actionTable.shift(0, Begin))
    val tokenWindow = mutable.Queue(Begin)

    val terminals = tokens.withFilter(whitespaceCommentFilter).map(token => TokenKind.kindToSymbol(token.kind))
    (terminals ++ Seq(End)).foreach {
      terminal =>

      // Keep track of the visited tokens for error purposes
        if (tokenWindow.size >= WindowSize) tokenWindow.dequeue()
        tokenWindow.enqueue(terminal)

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
        if (!actionTable.isShift(stateStack.top, terminal)) {
          throw new JoosParseException(s"No rule to parse with at ${tokenWindow.toString()}")
        }
        stateStack.push(actionTable.shift(stateStack.top, terminal))
    }

    assert(nodeStack.length == 3)
    ParseTree(nodeStack(1))
  }

  def whitespaceCommentFilter(token: Token) = {
    token.kind != TokenKind.Whitespace && token.kind != TokenKind.EolComment && token.kind != TokenKind
      .TraditionalComment
  }

}

object ParseTreeBuilder {
  def apply(actionTable: LrOneActionTable): ParseTreeBuilder = {
    new ParseTreeBuilder(actionTable)
  }
}
