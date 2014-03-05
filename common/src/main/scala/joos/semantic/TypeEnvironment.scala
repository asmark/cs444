package joos.semantic

import joos.ast.declarations.{FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  val constructorMap = mutable.HashMap.empty[String, MethodDeclaration]
  val methodMap = mutable.HashMap.empty[String, MethodDeclaration]
  val fieldMap = mutable.HashMap.empty[SimpleNameExpression, FieldDeclaration]

  /**
   * Adds the specified {{method}} to the type environment
   */
  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      assert(constructorMap.put(method.typedSignature, method).isEmpty)
    } else {
      assert(methodMap.put(method.typedSignature, method).isEmpty)
    }
    this
  }


  /**
   * Adds the specified {{field}} to the type environment
   */
  def add(field: FieldDeclaration): this.type = {
    assert(fieldMap.put(field.fragment.identifier, field).isEmpty)
    this
  }
}
