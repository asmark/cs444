package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class WhileStatement(condition: Expression, body: Statement) extends Statement

object WhileStatement {
  def apply(ptn: ParseTreeNode): WhileStatement = {
    null
  }
}