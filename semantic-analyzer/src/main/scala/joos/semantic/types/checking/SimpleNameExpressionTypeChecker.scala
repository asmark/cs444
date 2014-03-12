package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.SimpleNameExpression

trait SimpleNameExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(simpleNameExpression: SimpleNameExpression) {
    // TODO:
  }
}
