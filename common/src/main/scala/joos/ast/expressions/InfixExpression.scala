package joos.ast.expressions

import joos.tokens.TerminalToken
import joos.parsetree.ParseTreeNode

case class InfixExpression(left: Expression, operator: TerminalToken, right: Expression) extends Expression

object InfixExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
