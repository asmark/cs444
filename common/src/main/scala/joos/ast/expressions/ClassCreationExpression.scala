package joos.ast.expressions

import joos.ast.Type
import joos.parsetree.ParseTreeNode

case class ClassCreationExpression(classType: Type, args: Seq[Expression]) extends Expression

object ClassCreationExpression {
  def apply(ptn: ParseTreeNode) = {
    null
  }
}
