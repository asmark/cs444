package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.InstanceOfExpression
import joos.semantic._
import joos.semantic.types.{InstanceOfExpressionException, CastExpressionException}
import joos.ast.types.{ArrayType, SimpleType, NullType, PrimitiveType}

trait InstanceOfExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(instanceOfExpression: InstanceOfExpression) {
    instanceOfExpression.expression.accept(this)

    require(instanceOfExpression.expression.declarationType != null)
    if (isAssignable(instanceOfExpression.classType, instanceOfExpression.expression.declarationType) ||
        isAssignable(instanceOfExpression.expression.declarationType, instanceOfExpression.classType)) {
      instanceOfExpression.classType match {
        case SimpleType(_) | ArrayType(_,_) | NullType() => {}
        case _ => {
          throw new InstanceOfExpressionException(
            s"${instanceOfExpression.expression.declarationType.standardName} to ${instanceOfExpression.classType.standardName}"
          )
        }
      }

      instanceOfExpression.classType match {
        case SimpleType(_) | ArrayType(_,_) => {}
        case _ => {
          throw new InstanceOfExpressionException(
            s"${instanceOfExpression.expression.declarationType.standardName} to ${instanceOfExpression.classType.standardName}"
          )
        }
      }

      instanceOfExpression.declarationType = instanceOfExpression.classType
      return
    }

    throw new InstanceOfExpressionException(
      s"${instanceOfExpression.expression.declarationType.standardName} to ${instanceOfExpression.classType.standardName}"
    )
  }
}
