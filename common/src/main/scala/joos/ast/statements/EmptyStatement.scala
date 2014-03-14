package joos.ast.statements

import joos.semantic.BlockEnvironment

object EmptyStatement extends Statement {
  override var environment: BlockEnvironment = null

  override def toString = ";"
}
