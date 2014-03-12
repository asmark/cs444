package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.ParenthesizedExpression

trait ParenthesizedExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(parenthesizedExpression: ParenthesizedExpression) {
    parenthesizedExpression.expression.accept(this)
    parenthesizedExpression.declarationType = parenthesizedExpression.expression.declarationType
  }
}
