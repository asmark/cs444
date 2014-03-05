package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions.Expression
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class IfStatement(condition: Expression, trueStatement: Statement, falseStatement: Option[Statement]) extends Statement

object IfStatement {
  def apply(ptn: ParseTreeNode): IfStatement = {
    ptn match {
      case TreeNode(ProductionRule("IfThenStatement", _), _, children) =>
        IfStatement(Expression(children(2)), Statement(children(4)), None)
      case TreeNode(ProductionRule("IfThenElseStatement" | "IfThenElseStatementNoShortIf", _), _, children) =>
        IfStatement(Expression(children(2)), Statement(children(4)), Some(Statement(children(6))))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create IfStatement"
      )
    }
  }
}
