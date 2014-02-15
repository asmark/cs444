package joos.ast

import joos.ast.expressions.{ClassCreationExpression, MethodInvocationExpression, AssignmentExpression, Expression}
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException
import joos.ast.declarations.VariableDeclaration

case class ExpressionStatement(expr: Expression) extends Statement

object ExpressionStatement {
  private def constructStatementExpression(statementExpression: ParseTreeNode): Expression = {
    statementExpression match {
      case TreeNode(ProductionRule("StatementExpression", Seq("Assignment")), _, children) =>
        return AssignmentExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("MethodInvocation")), _, children) =>
        return MethodInvocationExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("ClassInstanceCreationExpression")), _, children) =>
        return ClassCreationExpression(children(0))
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        return VariableDeclaration(children(0))
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