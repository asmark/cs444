package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.PackageDeclaration
import joos.ast.expressions.NameExpression
import scala.collection.mutable

class ModuleEnvironment extends Environment {
  private[this] final val DefaultPackage = PackageDeclaration("")(this)

  /**
   * All packages declared within this module
   */
  private[this] val packages = mutable.LinkedHashMap.empty[NameExpression, PackageDeclaration]

  def add(compilationUnit: CompilationUnit) = {
    // TODO: a package is only visible if it contains a visible declaration?
    val packageDeclaration = compilationUnit.pkg match {
      case None => DefaultPackage
      case Some(pkg) => pkg
    }
    packages.put(packageDeclaration.name, packageDeclaration)
    compilationUnit.typeDeclaration match {
      case None =>
      case Some(t) => packageDeclaration.environment.add(t)
    }
    this
  }

  def nameToPackage: collection.Map[NameExpression, PackageDeclaration] = packages
}
