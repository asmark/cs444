package joos.analysis

import joos.ast.statements.{IfStatement, Block, Statement}
import scala.language.implicitConversions

package object reachability {
  implicit def toReachable(statement: Statement): Reachable = {
    statement match {
      case statement: Block => new ReachableBlock(statement)
      case statement: IfStatement => new ReachableIfStatement(statement)
    }
  }
}
