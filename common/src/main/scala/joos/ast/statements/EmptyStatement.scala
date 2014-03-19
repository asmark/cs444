package joos.ast.statements

import joos.semantic.BlockEnvironment

object EmptyStatement extends Statement {
  override def toString = ";"
}
