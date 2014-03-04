package joos.semantic

import joos.ast.declarations.{FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.SimpleNameExpression
import scala.collection.mutable

trait TypeEnvironment extends Environment {
  val constructors = mutable.ArrayBuffer.empty[MethodDeclaration]
  val methodMap = mutable.HashMap.empty[SimpleNameExpression, MethodDeclaration]
  val fieldMap = mutable.HashMap.empty[SimpleNameExpression, FieldDeclaration]

  /**
   * Adds the specified {{method}} to the type environment
   */
  def add(method: MethodDeclaration): this.type = {
    if (method.isConstructor) {
      constructors += method
    } else {
      assert(methodMap.put(method.name, method).isEmpty)
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
