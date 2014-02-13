package joos.ast

import joos.ast.expressions.Expression
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class ExpressionStatement(expr: Expression) extends Statement

object ExpressionStatement {
  def constructStatementExpression(statementExpression: ParseTreeNode): Expression = {
    statementExpression match {
      case TreeNode(ProductionRule("StatementExpression", Seq("Assignment")), _, children) =>
        return null // TODO: Return AssignmentExpression
      case TreeNode(ProductionRule("StatementExpression", Seq("MethodInvocation")), _, children) =>
        return null // TODO: Return MethodInvocationExpression
      case TreeNode(ProductionRule("StatementExpression", Seq("ClassInstanceCreationExpression")), _, children) =>
        return null // TODO: Return ClassCreationExpression
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        return null // TODO: Return LocalVariableDeclaration
      case _ => throw new AstConstructionException(
        "Invalid tree node to create StatementExpression"
      )
    }
  }

  def apply(ptn: ParseTreeNode): ExpressionStatement = {
    ptn match {
      case TreeNode(ProductionRule("ExpressionStatement", Seq("StatementExpression", ";")), _, children) =>
        return ExpressionStatement(constructStatementExpression(children(0)))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create ExpressionStatement"
      )
    }
  }
}