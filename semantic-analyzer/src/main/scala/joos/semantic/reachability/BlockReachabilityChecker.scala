package joos.semantic.reachability

import joos.ast.visitor.AstVisitor
import joos.ast.statements.Block

trait BlockReachabilityChecker extends AstVisitor {
  self: ReachabilityChecker =>

  override def apply(block: Block) {

  }
}
