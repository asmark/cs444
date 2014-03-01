package joos.ast

import joos.ast.declarations.ModuleDeclaration


trait CompilationUnitLinker {
  self: CompilationUnit =>

  def link(implicit moduleDeclaration: ModuleDeclaration): this.type = {
    implicit val that = this
    implicit val packageDeclaration = this.packageDeclaration
    this.moduleDeclaration = moduleDeclaration.add(this)
    this.typeDeclaration.map(_.link)
    this
  }
}
