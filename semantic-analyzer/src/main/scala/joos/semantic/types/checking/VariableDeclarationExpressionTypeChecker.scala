package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.VariableDeclarationExpression

trait VariableDeclarationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(variableDeclarationExpression: VariableDeclarationExpression) {
    // TODO:
  }
}
