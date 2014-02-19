package joos.ast

import joos.ast.expressions.NameExpression
import joos.parsetree.ParseTreeNode

case class SimpleType(val name: NameExpression) extends Type

object SimpleType {
  // TODO: Change it to ClassOrInterfaceType??
  def apply(ptn: ParseTreeNode): SimpleType = SimpleType(NameExpression(ptn))
}
