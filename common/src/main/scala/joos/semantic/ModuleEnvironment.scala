package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{TypeDeclaration, PackageDeclaration}
import joos.ast.expressions.NameExpression
import scala.collection.mutable

class ModuleEnvironment extends Environment {
  /**
   * All packages declared within this module
   */
  private[this] val packages = mutable.LinkedHashMap.empty[NameExpression, PackageDeclaration]

//  def add(compilationUnit: CompilationUnit) = {
//    // TODO: a package is only visible if it contains a visible declaration?
//    val packageDeclaration = compilationUnit.packageDeclaration match {
//      case None => ModuleEnvironment.DefaultPackage
//      case Some(pkg) => pkg
//    }
//    packages.put(packageDeclaration.name, packageDeclaration)
//    compilationUnit.typeDeclaration match {
//      case None =>
//      case Some(t) => packageDeclaration.environment.add(t)
//    }
//    this
//  }

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

//  def nameToPackage: collection.Map[NameExpression, PackageDeclaration] = packages
}

object ModuleEnvironment {
  final val DefaultPackage = PackageDeclaration("")
}
