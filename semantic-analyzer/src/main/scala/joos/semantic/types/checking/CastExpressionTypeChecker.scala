package joos.semantic.types.checking

import joos.ast.expressions.CastExpression

trait CastExpressionTypeChecker {
  self: TypeChecker =>
  override def apply(castExpression: CastExpression) {

  }
}
