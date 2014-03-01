package joos.ast

import joos.ast.exceptions.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}

case class Block(statements: Seq[Statement]) extends Statement

object Block {
  private def unfoldStatements(blockStatements: ParseTreeNode): Seq[Statement] = {
    //    for(node <- statementsNode.children)
    //      yield Statement(node.children(0)) // LocalVariableDeclarationStatement | Statement
    blockStatements match {
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatement")), _, children) =>
        Seq(Statement(children(0).children(0)))
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatements", "BlockStatement")), _, children) =>
        unfoldStatements(children(0)) ++ Seq(Statement(children(1).children(0)))
      case _ => throw new AstConstructionException("Invalid tree node to create BlockStatements")
    }
  }

  def apply(ptn: ParseTreeNode): Block = {
    ptn match {
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "BlockStatements", "}")), _, children) =>
        Block(unfoldStatements(children(1)))
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "}")), _, children) =>
        Block(Seq())
      case _ => throw new AstConstructionException("Invalid tree node to create Block")
    }
  }
}
