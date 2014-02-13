package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class IfStatement(condition: Expression, tStatement: Statement, fStatement: Statement) extends Statement

object IfStatement {
  def apply(ptn: ParseTreeNode): IfStatement = {
    null
  }
}