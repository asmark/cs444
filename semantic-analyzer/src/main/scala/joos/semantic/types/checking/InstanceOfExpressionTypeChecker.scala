package joos.semantic.types.checking

import joos.ast.expressions.InstanceOfExpression
import joos.ast.types.PrimitiveType._
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.semantic.types.TypeCheckingException

trait InstanceOfExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(expression: InstanceOfExpression) {
    expression.expression.accept(this)
    require(expression.expression.declarationType != null)

    if (!expression.classType.isReferenceType || !expression.expression.declarationType.isReferenceType) {
      throw new TypeCheckingException(
        "instanceof",
        s"Both operands need to be reference types instead of ${expression.classType.standardName} ${
          expression
              .expression
              .declarationType
              .standardName
        }")
    }

    if (isAssignable(expression.classType, expression.expression.declarationType)
        || isAssignable(expression.expression.declarationType, expression.classType)) {
      expression.declarationType = BooleanType
    } else {
      throw new TypeCheckingException(
        "instanceof",
        s"${expression.classType.standardName} are not related types ${expression.expression.declarationType.standardName}")
    }
  }
}
