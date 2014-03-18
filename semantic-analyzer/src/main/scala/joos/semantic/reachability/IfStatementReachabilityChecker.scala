package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.IfStatement

trait IfStatementReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>

  override def apply(ifStatement: IfStatement) {

  }
}
