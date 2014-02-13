package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class ReturnStatement(exp: Expression) extends Statement

object ReturnStatement {
  def apply(ptn: ParseTreeNode): ReturnStatement = {
    null
  }
}