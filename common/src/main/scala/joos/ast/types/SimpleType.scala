package joos.ast

import joos.ast.expressions.NameExpression
import joos.parsetree.ParseTreeNode
import joos.semantic.{BlockEnvironment, TypeEnvironment, ModuleEnvironment}

case class SimpleType(val name: NameExpression) extends Type

object SimpleType {
  // TODO: Change it to ClassOrInterfaceType??
  def apply(ptn: ParseTreeNode)(
      implicit moduleEnvironment: ModuleEnvironment,
      typeEnvironment: TypeEnvironment,
      blockEnvironment: BlockEnvironment): SimpleType = {
    SimpleType(NameExpression(ptn))
  }
}
