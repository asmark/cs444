package joos.semantic

import joos.ast.declarations.{PackageDeclaration, TypeDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait PackageEnvironment {

  self: PackageDeclaration =>

  /**
   * All types declared within this package
   */
  private[this] val types = mutable.HashMap.empty[SimpleNameExpression, TypeDeclaration]

  /**
   * Adds {typeDeclaration} to this package
   */
  def add(typeDeclaration: TypeDeclaration): this.type = {
    if (types.contains(typeDeclaration.name)) {
      throw new DuplicatedDeclarationException(typeDeclaration.name)
    }
    types.put(typeDeclaration.name, typeDeclaration)
    this
  }

  /**
   * Gets the type with the {{name}} within this package if it exists
   */
  def getType(name: SimpleNameExpression): Option[TypeDeclaration] = {
    types.get(name)
  }

  def getTypeDeclarations = types.values

  /**
   * For unit tests only
   */
  def clearEnvironment() = types.clear()
}
