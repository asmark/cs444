package joos.ast

case class CompilationUnit(
  pkg: Option[PackageDeclaration],
  imports: Seq[ImportDeclaration],
  typeDecl: TypeDeclaration
) extends AstNode
