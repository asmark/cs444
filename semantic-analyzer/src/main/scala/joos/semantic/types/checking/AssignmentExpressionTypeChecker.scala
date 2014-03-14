package joos.semantic.types.checking

import joos.ast.expressions.AssignmentExpression
import joos.ast.visitor.AstVisitor
import joos.semantic._
import joos.semantic.types.AssignmentExpressionException

trait AssignmentExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(assignmentExpression: AssignmentExpression) {
    assignmentExpression.left.accept(this)
    assignmentExpression.right.accept(this)

    require(assignmentExpression.left.declarationType != null)
    require(assignmentExpression.right.declarationType != null)
    val leftType = assignmentExpression.left.declarationType
    val rightType = assignmentExpression.right.declarationType
    // TODO: Double check if the following are complete (probably not)
    if (isAssignable(leftType, rightType)) {
      assignmentExpression.declarationType = leftType
    } else {
      throw new AssignmentExpressionException(s"${rightType.standardName} to ${leftType.standardName}")
    }
  }
}

