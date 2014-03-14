package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.ParenthesizedExpression

trait ParenthesizedExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(parenthesizedExpression: ParenthesizedExpression) {
    parenthesizedExpression.expression.accept(this)
    require(parenthesizedExpression.expression.declarationType != null)
    parenthesizedExpression.declarationType = parenthesizedExpression.expression.declarationType
  }
}
