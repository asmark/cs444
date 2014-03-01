package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

trait Statement extends AstNode

object Statement {
  private def handleStatementWithoutTrailingSubstatement(ptn: ParseTreeNode): Statement = {
    ptn match {
      case TreeNode(ProductionRule("StatementWithoutTrailingSubstatement", Seq("Block")), _, children) =>
        Block(children(0))
      case TreeNode(ProductionRule("StatementWithoutTrailingSubstatement", Seq("EmptyStatement")), _, children) =>
        EmptyStatement(children(0))
      case TreeNode(ProductionRule("StatementWithoutTrailingSubstatement", Seq("ExpressionStatement")), _, children) =>
        ExpressionStatement(children(0))
      case TreeNode(ProductionRule("StatementWithoutTrailingSubstatement", Seq("ReturnStatement")), _, children) =>
        ReturnStatement(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create StatementWithoutTrailingSubstatement"
      )
    }
  }

  private def handleLocalVariableDeclaration(node: ParseTreeNode): Statement = {
    node match {
      case TreeNode(ProductionRule("LocalVariableDeclaration", Seq("Type", "VariableDeclarator")), _, children) =>
        ExpressionStatement(node)
      case _ => throw new AstConstructionException(
        "Invalid tree node to create LocalVariableDeclaration"
      )
    }
  }

  // Extra cases to dispatch
  // StatementNoShortIf
  // LocalVariableDeclarationStatement
  def apply(ptn: ParseTreeNode): Statement = {
    ptn match {
      case TreeNode(ProductionRule("Statement", Seq("StatementWithoutTrailingSubstatement")), _, children) =>
        handleStatementWithoutTrailingSubstatement(children(0))
      case TreeNode(ProductionRule("Statement", Seq("IfThenStatement")), _, children) =>
        IfStatement(children(0))
      case TreeNode(ProductionRule("Statement", Seq("IfThenElseStatement")), _, children) =>
        IfStatement(children(0))
      case TreeNode(ProductionRule("Statement", Seq("WhileStatement")), _, children) =>
        WhileStatement(children(0))
      case TreeNode(ProductionRule("Statement", Seq("ForStatement")), _, children) =>
        ForStatement(children(0))
      case TreeNode(ProductionRule("StatementNoShortIf", Seq("StatementWithoutTrailingSubstatement")), _, children) =>
        handleStatementWithoutTrailingSubstatement(children(0))
      case TreeNode(ProductionRule("StatementNoShortIf", Seq("IfThenElseStatementNoShortIf")), _, children) =>
        IfStatement(children(0))
      case TreeNode(ProductionRule("StatementNoShortIf", Seq("WhileStatementNoShortIf")), _, children) =>
        WhileStatement(children(0))
      case TreeNode(ProductionRule("StatementNoShortIf", Seq("ForStatementNoShortIf")), _, children) =>
        ForStatement(children(0))
      case TreeNode(ProductionRule("LocalVariableDeclarationStatement", Seq("LocalVariableDeclaration", ";")), _, children) =>
        handleLocalVariableDeclaration(children(0))
      case _ => throw new AstConstructionException(
        "Invalid tree node to create Statement"
      )
    }
  }
}