package joos.ast.declarations

import joos.ast.CompilationUnit

class ModuleDeclaration {

  /**
   * Adds the {{compilationUnit}} to the module
   */
  def add(compilationUnit: CompilationUnit): this.type = {
    // TODO
    this
  }

  /**
   * Gets the package with the {{name}} if it exists
   */
  def getPackage(name: String): Option[PackageDeclaration] = {
    // TODO
    None
  }

  /**
   * Gets the type by its fully qualified name if it exists
   */
  def getType(name: String): Option[TypeDeclaration] = {
    None
  }
}

object ModuleDeclaration {
  final val DefaultPackage = PackageDeclaration("")
}
