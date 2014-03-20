package joos.analysis.reachability

import joos.analysis.reachability.ExpressionEvaluator._
import joos.ast.expressions.Expression
import joos.core.TernaryBoolean._
import scala.Some

trait ReachableLoop extends Reachable {
  protected def verify(condition: Expression, body: Reachable) {
    evaluate(condition) match {
      case None =>
        // canStart[body] = canStart[statement]
        // canFinish[statement] = canFinish[statement]
        body.canStart = this.canStart
        body.verify()
        this.canFinish = this.canFinish
      case Some(true) =>
        // canStart[body] = canStart[statement]
        // canFinish[statement] = false
        body.canStart = this.canStart
        body.verify()
        this.canFinish = False
      case Some(false) =>
        // canStart[body] = false
        // canFinish[statement] = canStart[statement]
        body.canStart = False
        body.verify()
        this.canFinish = this.canStart
      case _ =>
    }
  }
}
