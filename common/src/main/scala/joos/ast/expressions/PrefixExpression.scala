package joos.ast.expressions

import joos.tokens.TerminalToken
import joos.parsetree.ParseTreeNode

case class PrefixExpression(operator: TerminalToken, operand: Expression) extends Expression

object PrefixExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
