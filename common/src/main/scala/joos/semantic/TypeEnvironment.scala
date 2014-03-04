package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import scala.collection.mutable
import scala.language.implicitConversions
import joos.ast.CompilationUnit

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>

  private[this] val constructors = mutable.HashSet.empty[MethodDeclaration]
  private[this] val methods1 = mutable.HashSet.empty[MethodDeclaration]
  private[this] val fields1 = mutable.HashMap.empty[NameExpression, FieldDeclaration]

  /**
   * Adds the specified method declaration to the type environment
   * @return true if method did not exist in type environment before, false if it did
   */
  def add(method: MethodDeclaration) = {
    if (method.isConstructor) {
      constructors.add(method)
    } else {
      methods1.add(method)
    }
  }

  /**
   * Adds the specified field to the type environment
   * @return true if the field did not exist in the type environment before, false if it did
   */
  def add(field: FieldDeclaration) = {
    fields1.put(field.fragment.identifier, field).isEmpty
  }

  /**
   * Gets the field by its {{name}}
   */
  def getField(name: SimpleNameExpression) = {
    fields1.get(name)
  }

  // TODO: Inherited?
  def getFields = fields1.values

  /**
   * Gets a method that matches the {{method}}'s name and parameter types passed in if it exists
   */
  def getMethod(method: MethodDeclaration)(implicit unit: CompilationUnit) = {
  }

  // TODO: Inherited?
  def getMethods = methods1

  /**
   * Gets a constructor that matches the {{constructor}}'s parameter types passed in if it exists
   */
  def getConstructor(constructor: MethodDeclaration)(implicit unit: CompilationUnit) = {
  }

  // TODO: Inherited?
  def getConstructors = constructors
}
