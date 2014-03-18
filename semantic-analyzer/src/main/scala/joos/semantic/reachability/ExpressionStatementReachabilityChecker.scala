package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.ExpressionStatement

trait ExpressionStatementReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>
  override def apply(expressionStatement: ExpressionStatement) {

  }
}
