package joos.semantic

import joos.ast.declarations.{FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.NameExpression
import scala.collection
import scala.collection.mutable

class TypeEnvironment extends Environment {
  private[this] val methods = mutable.HashMap.empty[NameExpression, List[MethodDeclaration]]
  private[this] val fields  = mutable.HashMap.empty[NameExpression, FieldDeclaration]

  def add(method: MethodDeclaration): this.type = {
    val list = methods.getOrElse(method.name, List.empty[MethodDeclaration])
    for (existingMethod <- list) {
      // TODO: check for valid method overloading
      // TODO: constructor
      if (existingMethod.name == method.name) {
        throw new DuplicatedDeclarationException(method.name)
      }
    }
    methods.put(method.name, method :: list)
    this
  }

  def add(field: FieldDeclaration): this.type = {
    if (fields.contains(field.fragment.identifier)) {
      throw new DuplicatedDeclarationException(field.fragment.identifier)
    }
    fields.put(field.fragment.identifier, field)
    this
  }

  def nameToMethods: collection.Map[NameExpression, List[MethodDeclaration]] = methods

  def nameToFields: collection.Map[NameExpression, FieldDeclaration] = fields
}
