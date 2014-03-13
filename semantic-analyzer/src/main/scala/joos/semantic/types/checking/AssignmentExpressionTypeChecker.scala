package joos.semantic.types.checking

import joos.ast.expressions.{InfixExpression, AssignmentExpression}
import joos.ast.visitor.AstVisitor

trait AssignmentExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(assignmentExpression: InfixExpression) {
    assignmentExpression.left.accept(this)
    assignmentExpression.right.accept(this)
    val leftType = assignmentExpression.left.declarationType
    val rightType = assignmentExpression.right.declarationType
    // TODO:
  }
}

