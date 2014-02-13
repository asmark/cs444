package joos.ast.expressions

import joos.ast.declarations.VariableDeclarationFragment
import joos.ast.{Modifier, Type}
import joos.parsetree.ParseTreeNode

case class VariableDeclarationExpression(
   modifiers: Seq[Modifier],
   varType: Type,
   declaration: VariableDeclarationFragment
 ) extends Expression

object VariableDeclarationExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}