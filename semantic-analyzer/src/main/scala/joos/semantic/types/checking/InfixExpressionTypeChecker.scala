package joos.semantic.types.checking

import joos.ast.Operator._
import joos.ast.expressions.InfixExpression
import joos.ast.types.PrimitiveType._
import joos.ast.types._
import joos.ast.visitor.AstVisitor
import joos.semantic.types.InfixExpressionException

trait InfixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(expression: InfixExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
    require(expression.left.declarationType != null && expression.right.declarationType != null)
    val leftType = expression.left.declarationType
    val rightType = expression.right.declarationType
    val types = (leftType, rightType)


    expression.declarationType = expression.operator match {
      case Plus =>
        // String concatenation if one of the types is 'String' and the other is not 'void'
        types match {
          case (StringType, StringType) => StringType
          case (StringType, rightType) if rightType != VoidType => StringType
          case (leftType, StringType) if leftType != VoidType => StringType
          case (leftType, rightType) if leftType.isNumeric && rightType.isNumeric => IntegerType
          case _ => throw new InfixExpressionException(expression)
        }
      case Minus | Multiply | Divide | Modulo =>
        types match {
          case (leftType, rightType) if leftType.isNumeric && rightType.isNumeric => IntegerType
          case _ => throw new InfixExpressionException(expression)
        }
      case Less | LessOrEqual | Greater | GreaterOrEqual =>
        types match {
          case (leftType, rightType) if leftType.isNumeric && rightType.isNumeric => BooleanType
          case _ => throw new InfixExpressionException(expression)
        }
      case Equal | NotEqual =>
        types match {
          case (BooleanType, BooleanType) => BooleanType
          case (NullType, NullType) => BooleanType
          case (leftType, rightType) if leftType.isNumeric && rightType.isNumeric => BooleanType
          case (leftType, rightType) if leftType.isReferenceType && rightType.isReferenceType => BooleanType
          case _ => throw new InfixExpressionException(expression)
        }
      case BitwiseAnd | BitwiseExclusiveOr | BitwiseInclusiveOr | ConditionalAnd | ConditionalOr =>
        types match {
          case (BooleanType, BooleanType) => BooleanType
          case _ => throw new InfixExpressionException(expression)
        }
    }
  }
}
