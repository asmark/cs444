package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import scala.collection.mutable
import scala.language.implicitConversions

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>

  private[this] val constructors = mutable.LinkedHashMap.empty[String, MethodDeclaration]
  private[this] val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  private[this] val fieldMap = mutable.HashMap.empty[NameExpression, FieldDeclaration]

  /**
   * Adds the specified method declaration to the type environment
   * @return true if method did not exist in type environment before, false if it did
   */
  def add(method: MethodDeclaration) = {
    if (method.isConstructor) {
      constructors.put(method.typedSignature, method).isEmpty
    } else {
      methodMap.put(method.typedSignature, method).isEmpty
    }
  }

  /**
   * Adds the specified field to the type environment
   * @return true if the field did not exist in the type environment before, false if it did
   */
  def add(field: FieldDeclaration) = {
    fieldMap.put(field.fragment.identifier, field).isEmpty
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
    methodMap.get(method.typedSignature)
  }

  /**
   * Gets a constructor that matches the {{constructor}}'s parameter types passed in if it exists
   */
  def getConstructor(constructor: MethodDeclaration): Option[MethodDeclaration] = {
    constructors.get(constructor.typedSignature)
  }
}
