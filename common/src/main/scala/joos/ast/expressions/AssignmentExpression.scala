package joos.ast.expressions

import joos.ast.{ArrayAccessExpressionLike, AstConstructionException}
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}

case class AssignmentExpression(left: Expression, right: Expression) extends Expression {
  override def toString = s"${left} = ${right}"

  lazy val isLeftArrayAccess: Boolean = {
    left match {
      case arrayAccess: ArrayAccessExpressionLike => arrayAccess.isArrayAccess
      case _ => false
    }
  }
}

object AssignmentExpression {
  def apply(ptn: ParseTreeNode): AssignmentExpression = {
    ptn match {
      case TreeNode(ProductionRule("Assignment", _), _, children) =>
        AssignmentExpression(Expression(children(0)), Expression(children(2)))
      case _ => throw new AstConstructionException("No valid production rule to make AssignmentExpression")
    }
  }
}
