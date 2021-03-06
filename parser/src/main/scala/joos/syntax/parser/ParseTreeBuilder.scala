package joos.syntax.parser

import joos.syntax.parsetree.LeafNode
import joos.syntax.parsetree.TreeNode
import joos.syntax.parsetree._
import joos.syntax.tokens.NonTerminalToken
import joos.syntax.tokens.TerminalToken
import joos.syntax.tokens._
import scala.collection.mutable

class ParseTreeBuilder(actionTable: LrOneActionTable) {

  private final val Begin = TerminalToken("bof", null)
  private final val End = TerminalToken("eof", null)
  private final val WindowSize = 10

  def build(tokens: Seq[TerminalToken]): ParseTree = {

    val nodeStack = mutable.Stack(LeafNode(Begin)): mutable.Stack[ParseTreeNode]
    val stateStack = mutable.Stack(actionTable.shift(0, Begin.symbol))
    val tokenWindow = mutable.Queue(Begin): mutable.Queue[Token]

    (tokens ++ Seq(End)).withFilter(whitespaceCommentFilter).foreach {
      token =>

      // Keep track of the visited tokens for error purposes
        if (tokenWindow.size >= WindowSize) tokenWindow.dequeue()
        tokenWindow.enqueue(token)

        val symbol = token.symbol
        // Reduce tokens while you are able to, looking ahead by one terminal [LR(1)]
        while (actionTable.isReduce(stateStack.top, symbol)) {
          val productionRule = actionTable.reduce(stateStack.top, symbol)

          val childNodes = Range(0, productionRule.derivation.length).map {
            i => stateStack.pop(); nodeStack.pop()
          }.reverse


          nodeStack.push(TreeNode(productionRule, NonTerminalToken(productionRule.base, productionRule.base), childNodes))
          stateStack.push(actionTable.shift(stateStack.top, productionRule.base))
        }

        nodeStack.push(LeafNode(token))
        if (!actionTable.isShift(stateStack.top, token.symbol)) {
          throw new JoosParseException(s"No rule to parse with at ${tokenWindow.toString()}")
        }
        stateStack.push(actionTable.shift(stateStack.top, token.symbol))
    }

    assert(nodeStack.length == 3)
    ParseTree(nodeStack(1))
  }

  def whitespaceCommentFilter(token: Token) = {
    token match {
      case terminal: TerminalToken => {
        terminal.kind != TokenKind.Whitespace && terminal.kind != TokenKind.EolComment && terminal.kind != TokenKind.TraditionalComment
      }
      case _ => true
    }
  }

}

object ParseTreeBuilder {
  def apply(actionTable: LrOneActionTable): ParseTreeBuilder = {
    new ParseTreeBuilder(actionTable)
  }
}
