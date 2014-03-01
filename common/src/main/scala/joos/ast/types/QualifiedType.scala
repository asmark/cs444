package joos.ast

import joos.ast.expressions.NameExpression
import joos.parsetree.ParseTreeNode

case class QualifiedType(qualifier: Type, name: NameExpression) extends Type

object QualifiedType {
  def apply(ptn: ParseTreeNode): QualifiedType = {
    // TODO
    null
  }
}
