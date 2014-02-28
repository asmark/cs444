package joos.semantic

import joos.ast.declarations.{FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.NameExpression
import scala.collection
import scala.collection.mutable

class TypeEnvironment extends EnvironmentWithVariable {
  private[this] var constructors = List[MethodDeclaration]()
  private[this] val methods      = mutable.HashMap.empty[NameExpression, List[MethodDeclaration]]
  private[this] val fields       = mutable.HashMap.empty[NameExpression, FieldDeclaration]

  def parentEnvironment = None

  def variables = fields

  def add(method: MethodDeclaration): this.type = {
    // TODO: check for valid method overloading
    if (method.isConstructor) {
      constructors = method :: constructors
    } else {
      val list = methods.getOrElse(method.name, List[MethodDeclaration]())
      for (existingMethod <- list) {
        if (existingMethod.name == method.name) {
          throw new DuplicatedDeclarationException(method.name)
        }
      }
      methods.put(method.name, method :: list)
    }
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
