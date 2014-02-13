package joos.ast.declarations

import joos.ast.{Type, Modifier}
import joos.parsetree.ParseTreeNode

case class FieldDeclaration(
   modifiers: Seq[Modifier],
   retType: Type,
   fragment: VariableDeclarationFragment
 ) extends BodyDeclaration

object FieldDeclaration {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}