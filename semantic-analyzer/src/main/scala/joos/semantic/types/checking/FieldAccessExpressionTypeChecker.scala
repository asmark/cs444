package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.FieldAccessExpression

trait FieldAccessExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(fieldAccessExpression: FieldAccessExpression) {
    val primaryType = fieldAccessExpression.expression.accept(this)
    val identifierType = fieldAccessExpression.identifier.accept(this)
    // TODO:
  }
}
