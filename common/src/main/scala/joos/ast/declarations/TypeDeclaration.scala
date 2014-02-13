package joos.ast.declarations

import joos.ast.Modifier
import joos.ast.expressions.NameExpression
import joos.parsetree.ParseTreeNode

case class TypeDeclaration(
   modifiers: Seq[Modifier],
   isInterface: Boolean,
   superType: NameExpression,
   superInterfaces: Seq[NameExpression],
   fields: Seq[FieldDeclaration],
   methods: Seq[MethodDeclaration]
 ) extends BodyDeclaration

object TypeDeclaration {
  def apply(ptn: ParseTreeNode): TypeDeclaration = {
    return null
  }
}