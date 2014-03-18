package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.WhileStatement

trait WhileStatementReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>

  override def apply(whileStatement: WhileStatement) {

  }
}
