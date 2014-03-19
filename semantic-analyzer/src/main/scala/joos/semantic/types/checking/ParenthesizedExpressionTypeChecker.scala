package joos.semantic.types.checking

import joos.ast.expressions.ParenthesizedExpression
import joos.ast.visitor.AstVisitor

trait ParenthesizedExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>

  override def apply(parenthesis: ParenthesizedExpression) {
    parenthesis.expression.accept(this)
    require(parenthesis.expression.expressionType != null)
    parenthesis.expressionType = parenthesis.expression.expressionType
  }
}
