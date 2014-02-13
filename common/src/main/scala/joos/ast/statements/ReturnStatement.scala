package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class ReturnStatement(expression: Expression) extends Statement

object ReturnStatement {
  def apply(ptn: ParseTreeNode): ReturnStatement = {
    null
  }
}