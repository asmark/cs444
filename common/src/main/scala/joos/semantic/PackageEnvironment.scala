package joos.semantic

import joos.ast.declarations.TypeDeclaration
import joos.ast.expressions.NameExpression
import scala.collection.mutable

class PackageEnvironment extends Environment {

  /**
   * All types declared within this package
   */
  private[this] val types = mutable.HashMap.empty[NameExpression, TypeDeclaration]

  def add(typeDeclaration: TypeDeclaration): this.type = {
    if (types.contains(typeDeclaration.name)) {
      throw new DuplicatedDeclarationException(typeDeclaration.name)
    }
    types.put(typeDeclaration.name, typeDeclaration)
    this
  }

  def nameToType: collection.Map[NameExpression, TypeDeclaration] = types
}
