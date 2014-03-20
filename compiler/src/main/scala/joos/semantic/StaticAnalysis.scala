package joos.semantic

import joos.analysis.reachability.ReachabilityVerifier
import joos.ast.AbstractSyntaxTree

object StaticAnalysis {

  private[this] val builders = List(
    ReachabilityVerifier
  )

  def apply(asts: Seq[AbstractSyntaxTree]) {
    for (builder <- builders) {
      for (ast <- asts) {
        builder.build(ast.root)()
      }
    }
  }
}
