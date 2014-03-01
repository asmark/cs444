package joos.ast

import joos.parsetree.ParseTreeNode

case class EmptyStatement() extends Statement

object EmptyStatement {
  def apply(ptn: ParseTreeNode): EmptyStatement = {
    EmptyStatement()
  }
}