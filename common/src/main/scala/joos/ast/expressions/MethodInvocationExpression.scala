package joos.ast.expressions

import joos.parsetree.ParseTreeNode

case class MethodInvocationExpression(
   expr: Option[Expression],
   methodName: SimpleNameExpression,
   args: Seq[Expression]
 ) extends Expression

object MethodInvocationExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}