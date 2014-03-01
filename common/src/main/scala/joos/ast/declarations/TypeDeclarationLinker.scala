package joos.ast.declarations

import joos.ast.CompilationUnit

trait TypeDeclarationLinker {
  self: TypeDeclaration =>

  def link(implicit compilationUnit: CompilationUnit, packageDeclaration: PackageDeclaration): this.type = {
    this.compilationUnit = compilationUnit
    this.packageDeclaration = packageDeclaration.add(this)
    this
  }
}
