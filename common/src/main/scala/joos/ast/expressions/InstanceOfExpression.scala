package joos.ast.expressions

import joos.ast.types.Type
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class InstanceOfExpression(expression: Expression, classType: Type) extends Expression {
  override def toString = s"${expression} instanceof ${classType}"
}

object InstanceOfExpression {
  def apply(ptn: ParseTreeNode): InstanceOfExpression = {
    ptn match {
      case TreeNode(ProductionRule("RelationalExpression", Seq(_, "instanceof", _)), _, children) => {
        InstanceOfExpression(Expression(children(0)), Type.handleReferenceType(children(2)))
      }
    }
  }
}
