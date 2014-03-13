package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.VariableDeclarationExpression
import joos.semantic._

trait VariableDeclarationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(variableDeclarationExpression: VariableDeclarationExpression) {
    variableDeclarationExpression.declaration.initializer match {
      // TODO: verify this is the only check needed
      case Some(initExpr) => isAssignable(variableDeclarationExpression.declarationType, initExpr.declarationType)
      case None => {}
    }
  }
}
