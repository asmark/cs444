package joos.ast

import joos.parsetree.ParseTreeNode
import joos.ast.declarations.TypeDeclaration

case class TypeDeclarationStatement(declaration: TypeDeclaration)

object TypeDeclarationStatement {
  def apply(ptn: ParseTreeNode): TypeDeclarationStatement = {
    null
  }
}
