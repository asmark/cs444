package joos.ast.declarations

import joos.ast.CompilationUnit
import joos.semantic.BlockEnvironment

trait MethodDeclarationLinker {
  self: MethodDeclaration =>

  def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration): this.type = {
    this.typeDeclaration = typeDeclaration
    this.compilationUnit = compilationUnit
    implicit val environment = BlockEnvironment()
    this.environment = environment
    this.body.map(_.link)
    this
  }
}
