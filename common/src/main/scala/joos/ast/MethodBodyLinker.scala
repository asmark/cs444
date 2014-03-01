package joos.ast

import joos.ast.declarations.TypeDeclaration
import joos.semantic.BlockEnvironment

trait MethodBodyLinker {
  var compilationUnit: CompilationUnit = null
  var typeDeclaration: TypeDeclaration = null
  var environment: BlockEnvironment = null

  def link(implicit compilationUnit: CompilationUnit, typeDeclaration: TypeDeclaration, enclosingBlock: BlockEnvironment): this.type = {
    this.environment = environment
    this.compilationUnit = compilationUnit
    this.typeDeclaration = typeDeclaration
    this
  }
}
