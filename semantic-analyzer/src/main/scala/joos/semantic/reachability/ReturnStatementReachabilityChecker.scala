package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.ReturnStatement

trait ReturnStatementReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>

  override def apply(returnStatement: ReturnStatement) {

  }
}
