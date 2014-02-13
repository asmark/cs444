package joos.ast

import joos.parsetree.ParseTreeNode
import joos.tokens.TerminalToken

case class Modifier(modifier: TerminalToken) extends AstNode

object Modifier {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
