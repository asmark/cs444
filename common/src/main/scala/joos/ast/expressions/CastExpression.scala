package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.ParseTreeNode

case class CastExpression(castType: Type, expr: Expression) extends Expression

object CastExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
