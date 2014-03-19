package joos.semantic.types.checking

import joos.ast.compositions.TypedDeclarationLike
import joos.ast.expressions.{ArrayAccessExpression, AssignmentExpression}
import joos.ast.visitor.AstVisitor
import joos.ast.{Modifier, DeclarationReference}
import joos.semantic._
import joos.semantic.types.AssignmentExpressionException

trait AssignmentExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(assignment: AssignmentExpression) {
    val left = assignment.left
    val right = assignment.right

    left.accept(this)
    right.accept(this)

    require(left.expressionType != null)
    require(right.expressionType != null)

    val leftType = left.expressionType
    val rightType = right.expressionType
    if (isAssignable(leftType, rightType)) {
      assignment.expressionType = leftType
    } else {
      throw new AssignmentExpressionException(s"${rightType.standardName} to ${leftType.standardName}")
    }

    // We also need to make sure the left-hand side is a non-final variable/field reference
    left match {
      case reference: DeclarationReference[_] =>
        reference.declaration match {
          case variable: TypedDeclarationLike =>
            if (variable.modifiers.contains(Modifier.Final))
              throw new AssignmentExpressionException(left, right)
          case _ => throw new AssignmentExpressionException(left, right)
        }
      case _: ArrayAccessExpression =>
      case _ => throw new AssignmentExpressionException(left, right)
    }
  }
}

