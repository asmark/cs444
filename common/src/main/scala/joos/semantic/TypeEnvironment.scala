package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>

  private[this] val constructors = mutable.LinkedHashMap.empty[String, MethodDeclaration]
  private[this] val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  private[this] val fieldMap = mutable.HashMap.empty[NameExpression, FieldDeclaration]

  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      if (constructors.contains(method.typedName)) {
        throw new DuplicatedDeclarationException(method.name)
      }
      constructors.put(method.typedName, method)
    } else {
      if (methodMap.contains(method.typedName)) {
        throw new DuplicatedDeclarationException(method.name)
      }
      methodMap.put(method.typedName, method)
    }
    this
  }

  def add(field: FieldDeclaration): this.type = {
    if (fields.contains(field.fragment.identifier)) {
      throw new DuplicatedDeclarationException(field.fragment.identifier)
    }
    fieldMap.put(field.fragment.identifier, field)
    this
  }

  /**
   * Gets the field by its {{name}}
   */
  def getField(name: SimpleNameExpression): Option[FieldDeclaration] = {
    fieldMap.get(name)
  }


  /**
   * Gets a method that matches the {{method}}'s name and parameter types passed in if it exists
   */
  def getMethod(method: MethodDeclaration): Option[MethodDeclaration] = {
    methodMap.get(method.typedName)
  }

  /**
   * Gets a constructor that matches the {{constructor}}'s parameter types passed in if it exists
   */
  def getConstructor(constructor: MethodDeclaration): Option[MethodDeclaration] = {
    constructors.get(constructor.typedName)
  }
}
