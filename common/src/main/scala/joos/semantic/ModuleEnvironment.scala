package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.ModuleDeclaration

trait ModuleEnvironment extends Environment {
  self: ModuleDeclaration =>

  val namespace = new NamespaceTrie

  /**
   * Adds the {{compilationUnit}} to the module
   */
  def add(compilationUnit: CompilationUnit): this.type = {
    val packageDeclaration = compilationUnit.packageDeclaration
    namespace.add(packageDeclaration.name, compilationUnit.typeDeclaration)
    this
  }

}
