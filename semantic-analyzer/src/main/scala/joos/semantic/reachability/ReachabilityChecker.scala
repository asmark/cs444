package joos.semantic.reachability

import joos.ast.CompilationUnit
import joos.semantic.types.AstEnvironmentVisitor

class ReachabilityChecker(implicit val unit: CompilationUnit)
  extends AstEnvironmentVisitor
  with ForStatementReachabilityChecker
  with ExpressionStatementReachabilityChecker
  with IfStatementReachabilityChecker
  with WhileStatementReachabilityChecker
  with ReturnStatementReachabilityChecker
  with BlockReachabilityChecker {

}
