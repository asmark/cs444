package joos.ast

import joos.ast.declarations.{TypeDeclaration, PackageDeclaration, ImportDeclaration}
import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.ModuleEnvironment

case class CompilationUnit(
    pkg: Option[PackageDeclaration],
    imports: Seq[ImportDeclaration],
    typeDeclaration: Option[TypeDeclaration])(
    implicit val moduleEnvironment: ModuleEnvironment) extends AstNode

object CompilationUnit {
  def apply(ptn: ParseTreeNode)(implicit moduleEnvironment: ModuleEnvironment): CompilationUnit = {
    val compilationUnit = ptn match {
      case TreeNode(ProductionRule("CompilationUnit", Seq()), _, children) =>
        CompilationUnit(None, Seq.empty, None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("TypeDeclaration")), _, children) =>
        CompilationUnit(None, Seq.empty, Some(TypeDeclaration(children(0))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("ImportDeclarations")), _, children) =>
        CompilationUnit(None, ImportDeclaration(children(0)), None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("ImportDeclarations", "TypeDeclaration")), _, children) =>
        CompilationUnit(None, ImportDeclaration(children(0)), Some(TypeDeclaration(children(1))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration")), _, children) =>
        CompilationUnit(Some(PackageDeclaration(children(0))), Seq.empty, None)
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration", "TypeDeclaration")), _, children) =>
        CompilationUnit(Some(PackageDeclaration(children(0))), Seq.empty, Some(TypeDeclaration(children(1))))
      case TreeNode(ProductionRule("CompilationUnit", Seq("PackageDeclaration", "ImportDeclarations")), _, children) =>
        CompilationUnit(Some(PackageDeclaration(children(0))), ImportDeclaration(children(1)), None)
      case TreeNode(
      ProductionRule(
      "CompilationUnit",
      Seq("PackageDeclaration", "ImportDeclarations", "TypeDeclaration")), _, children) =>
        CompilationUnit(
          Some(PackageDeclaration(children(0))),
          ImportDeclaration(children(1)),
          Some(TypeDeclaration(children(2)))
        )
      case _ => throw new AstConstructionException(
        "No valid production rule to create CompilationUnit"
      )
    }
    moduleEnvironment.add(compilationUnit)
    compilationUnit
  }
}
