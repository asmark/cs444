package joos.semantic.types.checking

import joos.ast.expressions.CastExpression
import joos.ast.visitor.AstVisitor
import joos.ast.types.PrimitiveType
import joos.semantic._
import joos.semantic.types.CastExpressionException

trait CastExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(castExpression: CastExpression) {
    if (castExpression.castType.isNumeric &&
        castExpression.expression.declarationType.isNumeric) {
      castExpression.declarationType = castExpression.castType
      return
    }

    if (isAssignable(castExpression.castType, castExpression.expression.declarationType) ||
        isAssignable(castExpression.expression.declarationType, castExpression.castType)) {
      castExpression.declarationType = castExpression.castType
      return
    }

    throw new CastExpressionException(s"${castExpression.expression.declarationType.standardName} to ${castExpression.castType.standardName}")
  }
}
