package joos.ast.expressions

import joos.parsetree.ParseTreeNode

case class ParenthesizedExpression(expression: Expression) extends Expression

object ParenthesizedExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
