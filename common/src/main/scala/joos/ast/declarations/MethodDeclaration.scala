package joos.ast.declarations

import joos.ast.expressions.SimpleNameExpression
import joos.ast.{Block, Type, Modifier}
import joos.parsetree.ParseTreeNode

case class MethodDeclaration(
  modifiers: Seq[Modifier],
  retType: Option[Type],
  retDims: Int,
  name: SimpleNameExpression,
  params: Seq[SingleVariableDeclaration],
  body: Block,
  isConstructor: Boolean
) extends BodyDeclaration

object MethodDeclaration {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}