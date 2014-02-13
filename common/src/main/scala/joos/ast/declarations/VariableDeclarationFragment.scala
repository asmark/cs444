package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.parsetree.ParseTreeNode

case class VariableDeclarationFragment(
  identifier: SimpleNameExpression,
  initializer: Option[Expression]
)

object VariableDeclarationFragment {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}