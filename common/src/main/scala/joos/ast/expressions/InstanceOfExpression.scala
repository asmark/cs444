package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.ParseTreeNode

case class InstanceOfExpression(expression: Expression, classType: Type) extends Expression

object InstanceOfExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
