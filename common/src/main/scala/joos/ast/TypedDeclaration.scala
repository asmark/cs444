package joos.ast

import joos.ast.expressions.NameExpression

/**
 * Represents a declaration that has a type
 */
trait TypedDeclaration {
  def declarationName: NameExpression
  def declarationType: Type
}
