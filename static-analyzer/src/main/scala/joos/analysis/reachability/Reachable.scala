package joos.analysis.reachability

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

  /**
   * Verify this statement follows the reachability rule
   */
  def verify(): Unit
}
