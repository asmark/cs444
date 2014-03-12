package joos.semantic.types.checking

import joos.ast.visitor.AstVisitor
import joos.ast.expressions.MethodInvocationExpression

/**
 * Created by freddie on 12/03/14.
 */
trait MethodInvocationExpressionTypeChecker extends AstVisitor {
  self: TypeChecker =>
  override def apply(methodInvocationExpression: MethodInvocationExpression) {

  }
}
