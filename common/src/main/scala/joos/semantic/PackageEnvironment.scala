package joos.semantic

import joos.ast.declarations.TypeDeclaration
import scala.collection.mutable

trait PackageEnvironment {

  /**
   * All types declared within this package
   */
  private[this] val types = mutable.HashMap.empty[String, TypeDeclaration]

  /**
   * Adds {typeDeclaration} to this package
   */
  def add(typeDeclaration: TypeDeclaration): this.type = {
    if (types.contains(typeDeclaration.name.standardName)) {
      throw new DuplicatedDeclarationException(typeDeclaration.name)
    }
    types.put(typeDeclaration.name.standardName, typeDeclaration)
    this
  }

  /**
   * Gets the type with the {{name}} within this package if it exists
   */
  def getType(name: String): Option[TypeDeclaration] = {
    types.get(name)
  }
}
