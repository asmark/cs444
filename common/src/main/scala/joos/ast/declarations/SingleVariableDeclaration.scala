package joos.ast.declarations

import joos.ast.expressions.{Expression, SimpleNameExpression}
import joos.ast.{Type, Modifier}
import joos.parsetree.ParseTreeNode

case class SingleVariableDeclaration(
   modifiers: Seq[Modifier],
   varType: Type,
   identifier: SimpleNameExpression,
   initializer: Option[Expression]
 ) extends VariableDeclaration

object SingleVariableDeclaration {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}