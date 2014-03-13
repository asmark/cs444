package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.InfixExpression

trait InfixExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(expression: InfixExpression) {
    expression.left.accept(this)
    expression.right.accept(this)
    val leftType = expression.left.declarationType
    val rightType = expression.right.declarationType
    // TODO:
  }
}
