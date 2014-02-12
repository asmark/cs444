package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class CompilationUnit(
  pkg: Option[PackageDeclaration],
  imports: Seq[ImportDeclaration],
  typeDecl: TypeDeclaration
) extends AstNode

object CompilationUnit {
  def apply(ptn: ParseTreeNode): CompilationUnit = {
    ptn match {
      case TreeNode(
      ProductionRule("CompilationUnit", Seq("PackageDeclarationopt", "ImportDeclarationsopt", "TypeDeclarationopt")),
      token,
      children
      )
      => return new CompilationUnit(
        PackageDeclaration(children(0)),
        ImportDeclaration(children(1)),
        TypeDeclaration(children(2))
      )
      case _ => throw new AstConstructionException(
        "No valid production rule to create CompilationUnit"
      )
    }
  }
}