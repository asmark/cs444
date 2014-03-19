package joos.semantic.types.checking

import joos.ast.expressions.InstanceOfExpression
import joos.ast.types.PrimitiveType._
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.semantic.types.TypeCheckingException

trait InstanceOfExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(instanceOf: InstanceOfExpression) {
    instanceOf.expression.accept(this)
    require(instanceOf.expression.expressionType != null)

    if (!instanceOf.classType.isReferenceType || !instanceOf.expression.expressionType.isReferenceType) {
      throw new TypeCheckingException(
        "instanceof",
        s"Both operands need to be reference types instead of ${instanceOf.classType.standardName} ${
          instanceOf
              .expression
              .expressionType
              .standardName
        }")
    }

    if (isAssignable(instanceOf.classType, instanceOf.expression.expressionType)
        || isAssignable(instanceOf.expression.expressionType, instanceOf.classType)) {
      instanceOf.expressionType = BooleanType
    } else {
      throw new TypeCheckingException(
        "instanceof",
        s"${instanceOf.classType.standardName} are not related types ${instanceOf.expression.expressionType.standardName}")
    }
  }
}
