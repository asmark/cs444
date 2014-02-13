package joos.ast

import joos.ast.expressions.NameExpression
import joos.parsetree.ParseTreeNode

case class SimpleType(name: NameExpression) extends Type

object SimpleType {
  def apply(ptn: ParseTreeNode): QualifiedType = {
    null
  }
}