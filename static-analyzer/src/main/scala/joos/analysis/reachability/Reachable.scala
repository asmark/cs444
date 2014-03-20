package joos.analysis.reachability

import joos.analysis.exceptions.UnreachableException
import joos.ast.statements.Statement
import joos.core.TernaryBoolean._

trait Reachable {
  /**
   * Whether this statement can start executing
   */
  protected[reachability] var canStart = Maybe

  /**
   * Whether this statement can finish executing
   */
  protected[reachability] var canFinish = Maybe

  def statement: Statement

  /**
   * Verify this statement follows the reachability rule
   */
  def verify() {
    if (canStart == False)
      throw new UnreachableException(statement)
  }
}
