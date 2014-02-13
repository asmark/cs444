package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.ParseTreeNode

case class ForStatement(forInit: Expression, cond: Expression, forUpdate: Expression, body: Statement) extends Statement

object ForStatement {
  def apply(ptn: ParseTreeNode): ForStatement = {
    null
  }
}