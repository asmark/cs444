package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.ParseTreeNode

case class InstanceOfExpression(expr: Expression, refType: Type) extends Expression

object InstanceOfExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
