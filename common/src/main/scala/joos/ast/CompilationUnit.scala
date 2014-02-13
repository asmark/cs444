package joos.ast

import joos.ast.declarations.{TypeDeclaration, PackageDeclaration, ImportDeclaration}
import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class CompilationUnit(
  pkg: Option[PackageDeclaration],
  imports: Seq[ImportDeclaration],
  typeDecl: Option[TypeDeclaration]
) extends AstNode

object CompilationUnit {
  def apply(ptn: ParseTreeNode): CompilationUnit = {
    ptn match {
      case TreeNode(ProductionRule("CompilationUnit", Seq()), _, children) =>
        return CompilationUnit(None, Seq.empty, None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("TypeDeclaration")),_, children) =>
        return CompilationUnit(None, Seq.empty, Some(TypeDeclaration(children(0))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("ImportDeclarations")),_, children) =>
        return CompilationUnit(None, ImportDeclaration(children(0)), None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("ImportDeclarations", "TypeDeclaration")),_, children) =>
        return CompilationUnit(None, ImportDeclaration(children(0)), Some(TypeDeclaration(children(1))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration")),_, children) =>
        return CompilationUnit(Some(PackageDeclaration(children(0))), Seq.empty, None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration", "TypeDeclaration")),_, children) =>
        return CompilationUnit(Some(PackageDeclaration(children(0))), Seq.empty, Some(TypeDeclaration(children(1))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration", "ImportDeclarations")), _, children) =>
        return CompilationUnit(Some(PackageDeclaration(children(0))), ImportDeclaration(children(1)), None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration", "ImportDeclarations", "TypeDeclaration")),_, children) =>
        return CompilationUnit(Some(PackageDeclaration(children(0))), ImportDeclaration(children(1)), Some(TypeDeclaration(children(2))))
      case _ => throw new AstConstructionException(
        "No valid production rule to create CompilationUnit"
      )
    }
  }
}