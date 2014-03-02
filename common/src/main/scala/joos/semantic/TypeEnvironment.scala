package joos.semantic

import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.{SimpleNameExpression, NameExpression}
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  self: TypeDeclaration =>

  private[this] val constructors = mutable.LinkedHashMap.empty[String, MethodDeclaration]
  private[this] val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  private[this] val fieldMap = mutable.HashMap.empty[NameExpression, FieldDeclaration]
  private[this] var extendedClass: Option[TypeDeclaration] = None
  private[this] val implementsMap = mutable.HashMap.empty[NameExpression, TypeDeclaration]

  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      if (constructors.contains(method.typedSignature)) {
        throw new DuplicatedDeclarationException(method.name)
      }
      constructors.put(method.typedSignature, method)
    } else {
      if (methodMap.contains(method.typedSignature)) {
        throw new DuplicatedDeclarationException(method.name)
      }
      methodMap.put(method.typedSignature, method)
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

  def add(ancestor: TypeDeclaration): this.type = {
    ancestor.isInterface match {
      case true => {
        if (implementsMap.contains(ancestor.name)) {
          throw new DuplicatedDeclarationException(ancestor.name)
        }
        implementsMap.put(ancestor.name, ancestor)
      }
      case false => {
        extendedClass match {
          case Some(superType) =>
            throw new DuplicatedDeclarationException(ancestor.name)
          case None =>
            extendedClass = Some(ancestor)
        }
      }
    }
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
    methodMap.get(method.typedSignature)
  }

  /**
   * Gets a constructor that matches the {{constructor}}'s parameter types passed in if it exists
   */
  def getConstructor(constructor: MethodDeclaration): Option[MethodDeclaration] = {
    constructors.get(constructor.typedSignature)
  }

  def getExtendedClass(): Option[TypeDeclaration] = {
    extendedClass
  }

  def getImplementedInterface(name: NameExpression): Option[TypeDeclaration] = {
    implementsMap.get(name)
  }

  def getAllImplementedInterfaces() = implementsMap.values
}
