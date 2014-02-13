package joos.ast.expressions

import joos.parsetree.ParseTreeNode

case class FieldAccessExpression(expr: Expression, identifier: SimpleNameExpression) extends Expression

object FieldAccessExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
