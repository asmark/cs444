package joos.ast.declarations

import joos.ast.expressions.SimpleNameExpression
import joos.ast.{Block, Type, Modifier}
import joos.parsetree.ParseTreeNode

case class MethodDeclaration(
  modifiers: Seq[Modifier],
  returnType: Option[Type],
  returnDims: Int,
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