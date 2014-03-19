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

    require(assignmentExpression.left.expressionType != null)
    require(assignmentExpression.right.expressionType != null)

    val leftType = assignmentExpression.left.expressionType
    val rightType = assignmentExpression.right.expressionType
    // TODO: Double check if the following are complete (probably not)
    if (isAssignable(leftType, rightType)) {
      assignmentExpression.expressionType = leftType
    } else {
      throw new AssignmentExpressionException(s"${rightType.standardName} to ${leftType.standardName}")
    }
  }
}

