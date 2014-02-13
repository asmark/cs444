package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class ForStatement(forInit: Expression, condition: Expression, forUpdate: Expression, body: Statement) extends Statement

object ForStatement {
  def apply(ptn: ParseTreeNode): ForStatement = {
    null
  }
}