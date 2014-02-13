package joos.ast.expressions

import joos.parsetree.ParseTreeNode

case class FieldAccessExpression(expression: Expression, identifier: SimpleNameExpression) extends Expression

object FieldAccessExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
