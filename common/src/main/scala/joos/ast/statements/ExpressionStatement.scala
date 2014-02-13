package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class ExpressionStatement(expression: Expression) extends Statement

object ExpressionStatement {
  def apply(ptn: ParseTreeNode): ExpressionStatement = {
    null
  }
}