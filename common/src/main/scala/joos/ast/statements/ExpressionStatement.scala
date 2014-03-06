package joos.ast.statements

import joos.ast.AstConstructionException
import joos.ast.expressions._
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.ParseTreeNode
import joos.syntax.parsetree.TreeNode

case class ExpressionStatement(expression: Expression) extends Statement

object ExpressionStatement {
  def constructStatementExpression(statementExpression: ParseTreeNode): Expression = {
    statementExpression match {
      case TreeNode(ProductionRule("StatementExpression", Seq("Assignment")), _, children) =>
        AssignmentExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("MethodInvocation")), _, children) =>
        MethodInvocationExpression(children(0))
      case TreeNode(ProductionRule("StatementExpression", Seq("ClassInstanceCreationExpression")), _, children) =>
        ClassCreationExpression(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create StatementExpression"
      )
    }
  }

  def apply(ptn: ParseTreeNode): ExpressionStatement = {
    ptn match {
      case TreeNode(ProductionRule("ExpressionStatement", Seq("StatementExpression", ";")), _, children) =>
        ExpressionStatement(constructStatementExpression(children(0)))
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        ExpressionStatement(VariableDeclarationExpression(ptn))
      case _ => throw new AstConstructionException("Invalid tree node to create ExpressionStatement")
    }
  }
}
