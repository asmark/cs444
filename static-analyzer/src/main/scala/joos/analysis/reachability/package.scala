package joos.analysis

import joos.ast.statements.{Block, Statement}
import scala.language.implicitConversions

package object reachability {
  implicit def toReachable(statement: Statement): Reachable = {
    statement match {
      case block: Block => new ReachableBlock(block)
    }
  }
}
