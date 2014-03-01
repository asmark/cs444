package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{ModuleDeclaration, TypeDeclaration, PackageDeclaration}
import scala.collection.mutable

trait ModuleEnvironment extends Environment {
  self: ModuleDeclaration =>

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

object ModuleEnvironment {
  final val DefaultPackage = PackageDeclaration("")
}
