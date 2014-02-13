package joos.ast

import joos.parsetree.ParseTreeNode
import joos.tokens.TerminalToken

case class PrimitiveType(primType: TerminalToken) extends Type

object PrimitiveType {
  def apply(ptn: ParseTreeNode): PrimitiveType = {
    null
  }
}