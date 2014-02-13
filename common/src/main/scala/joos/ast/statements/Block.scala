package joos.ast

import joos.parsetree.ParseTreeNode

case class Block(inner: Seq[Statement]) extends Statement

object Block {
  def apply(ptn: ParseTreeNode): Block = {
    null
  }
}
