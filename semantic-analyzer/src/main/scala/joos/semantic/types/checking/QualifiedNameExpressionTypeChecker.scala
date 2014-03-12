package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.QualifiedNameExpression

trait QualifiedNameExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(qualifiedNameExpression: QualifiedNameExpression) {
    // TODO:
  }
}
