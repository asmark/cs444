package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class Block(inner: Option[Seq[Statement]]) extends Statement

object Block {
  private def unfoldStatements(blockStatements: ParseTreeNode): Seq[Statement] = {
//    for(node <- statementsNode.children)
//      yield Statement(node.children(0)) // LocalVariableDeclarationStatement | Statement
    blockStatements match {
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatement")), _, children) =>
        return Seq(Statement(children(0).children(0)))
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatements", "BlockStatement")), _, children) =>
        return unfoldStatements(children(0)) ++ Seq(Statement(children(1).children(0)))
      case _ => throw new AstConstructionException("Invalid tree node to create BlockStatements")
    }
  }

  def apply(ptn: ParseTreeNode): Block = {
    ptn match {
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "BlockStatements", "}")), _, children) =>
        return new Block(Some(unfoldStatements(children(1))))
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "}")), _, children) =>
        return new Block(None)
      case _ => throw new AstConstructionException("Invalid tree node to create Block")
    }
  }
}
