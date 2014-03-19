package joos.semantic.types.checking

import joos.ast.expressions.CastExpression
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.semantic.types.CastExpressionException

trait CastExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(castExpression: CastExpression) {
    castExpression.expression.accept(this)

    require(castExpression.expression.expressionType != null)

    if (castExpression.castType.isNumeric &&
        castExpression.expression.expressionType.isNumeric) {
      castExpression.expressionType = castExpression.castType
      return
    }

    if (isAssignable(castExpression.castType, castExpression.expression.expressionType) ||
        isAssignable(castExpression.expression.expressionType, castExpression.castType)) {
      castExpression.expressionType = castExpression.castType
      return
    }

    throw new CastExpressionException(s"${castExpression.expression.expressionType.standardName} to ${castExpression.castType.standardName}")
  }
}
