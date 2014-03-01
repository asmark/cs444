package joos.ast.expressions

import joos.ast.Type
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class InstanceOfExpression(expression: Expression, classType: Type) extends Expression

object InstanceOfExpression {
  def apply(ptn: ParseTreeNode): InstanceOfExpression = {
    ptn match {
      case TreeNode(ProductionRule("RelationalExpression", Seq(_, "instanceof", _)), _, children) => {
        InstanceOfExpression(Expression(children(0)), Type.handleReferenceType(children(2)))
      }
    }
  }
}
