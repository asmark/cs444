package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{ModuleDeclaration, TypeDeclaration, PackageDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, NameExpression}
import scala.collection.mutable

trait ModuleEnvironment extends Environment {
  self: ModuleDeclaration =>

  private[this] val packages = mutable.HashMap.empty[NameExpression, PackageDeclaration]

  /**
   * Adds the {{compilationUnit}} to the module
   */
  def add(compilationUnit: CompilationUnit): this.type = {
    val packageDeclaration = compilationUnit.packageDeclaration
    packages.put(packageDeclaration.name, packageDeclaration)
    this
  }

  /**
   * Gets the package with the {{name}} if it exists
   */
  def getPackage(name: NameExpression): Option[PackageDeclaration] = {
    packages.get(name)
  }

  /**
   * Gets the type by its fully qualified name if it exists
   */
  def getType(name: NameExpression): Option[TypeDeclaration] = {
    name match {
      case qualifiedName: QualifiedNameExpression => packages.get(qualifiedName.qualifier) flatMap (_.getType(qualifiedName.name))
      case _ => None
    }
  }
}
