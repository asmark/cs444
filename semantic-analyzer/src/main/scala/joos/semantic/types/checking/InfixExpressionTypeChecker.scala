package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.InfixExpression

trait InfixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(infixExpression: InfixExpression) {
    infixExpression.left.accept(this)
    infixExpression.right.accept(this)
    val leftType = infixExpression.left.declarationType
    val rightType = infixExpression.right.declarationType
    // TODO:
  }
}
