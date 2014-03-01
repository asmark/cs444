package joos.ast

import joos.parsetree.ParseTreeNode
import joos.ast.declarations.TypeDeclaration

case class TypeDeclarationStatement(typeDeclaration: TypeDeclaration) extends Statement

object TypeDeclarationStatement {
  def apply(ptn: ParseTreeNode): TypeDeclarationStatement = {
    TypeDeclarationStatement(TypeDeclaration(ptn))
  }
}
