package joos.semantic

import joos.ast.CompilationUnit
import joos.ast.declarations.{ModuleDeclaration, TypeDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, NameExpression}
import scala.collection.mutable

trait ModuleEnvironment extends Environment {
  self: ModuleDeclaration =>

  private[this] val packages = mutable.HashMap.empty[NameExpression, PackageEnvironment]

  /**
   * Adds the {{compilationUnit}} to the module
   */
  def add(compilationUnit: CompilationUnit): this.type = {
    val packageDeclaration = compilationUnit.packageDeclaration
    val packageEnvironment = packages.getOrElse(packageDeclaration.name, new PackageEnvironment)
    compilationUnit.typeDeclaration map (packageEnvironment.add(_))
    packages.put(packageDeclaration.name, packageEnvironment)
    this

  }

  /**
   * Gets the package with the {{name}} if it exists
   */
  def getPackageEnvironment(name: NameExpression): Option[PackageEnvironment] = {
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
