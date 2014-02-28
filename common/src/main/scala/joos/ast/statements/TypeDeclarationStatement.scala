package joos.ast

import joos.ast.declarations.TypeDeclaration
import joos.parsetree.ParseTreeNode
import joos.semantic.ModuleEnvironment

case class TypeDeclarationStatement(typeDeclaration: TypeDeclaration) extends Statement

object TypeDeclarationStatement {
  def apply(ptn: ParseTreeNode)(implicit moduleEnvironment: ModuleEnvironment): TypeDeclarationStatement = {
    return TypeDeclarationStatement(TypeDeclaration(ptn))
  }
}
