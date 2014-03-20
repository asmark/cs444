package joos.semantic

import joos.analysis.reachability.ReachabilityChecker
import joos.ast.AbstractSyntaxTree

object StaticAnalysis {

  private[this] val builders = List(
    ReachabilityChecker
  )

  def apply(asts: Seq[AbstractSyntaxTree]) {
    for (builder <- builders) {
      for (ast <- asts) {
        builder.build(ast.root)()
      }
    }
  }
}
