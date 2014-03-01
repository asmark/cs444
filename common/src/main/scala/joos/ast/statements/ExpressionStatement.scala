package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.ast.expressions._
import joos.language.ProductionRule
import joos.parsetree.ParseTreeNode
import joos.parsetree.TreeNode

case class ExpressionStatement(expr: Expression) extends Statement

object ExpressionStatement {
  def constructStatementExpression(statementExpression: ParseTreeNode): Expression = {
    statementExpression match {
      case TreeNode(ProductionRule("StatementExpression", Seq("Assignment")), _, children) =>
        return AssignmentExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("MethodInvocation")), _, children) =>
        return MethodInvocationExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("ClassInstanceCreationExpression")), _, children) =>
        return ClassCreationExpression(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create StatementExpression"
      )
    }
  }

  def apply(ptn: ParseTreeNode): ExpressionStatement = {
    ptn match {
      case TreeNode(ProductionRule("ExpressionStatement", Seq("StatementExpression", ";")), _, children) =>
        return ExpressionStatement(constructStatementExpression(children(0)))
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        return ExpressionStatement(VariableDeclarationExpression(ptn))
      case _ => throw new AstConstructionException("Invalid tree node to create ExpressionStatement")
    }
  }
}