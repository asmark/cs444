package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.ForStatement

trait ForStatementReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>

  override def apply(forStatement: ForStatement) {

  }
}
