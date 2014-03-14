package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions.Expression
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.BlockEnvironment

case class IfStatement(condition: Expression, trueStatement: Statement, falseStatement: Option[Statement]) extends Statement {
  override var environment: BlockEnvironment = null

  override def toString = {
    val suffix = falseStatement match {
      case None => ""
      case Some(statement) => s" else {\n${statement}\n}"
    }
    s"if (${condition}) {\n${trueStatement}\n}${suffix}\n"
  }
}

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
