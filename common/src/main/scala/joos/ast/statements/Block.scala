package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class Block(inner: Option[Seq[Statement]]) extends Statement

object Block {
  private def unfoldStatements(statementsNode: ParseTreeNode): Seq[Statement] = {
    for(node <- statementsNode.children)
      yield Statement(node.children(0)) // LocalVariableDeclarationStatement | Statement
  }

  def apply(ptn: ParseTreeNode): Block = {
    ptn match {
      case TreeNode(ProductionRule("Block", Seq("{", "BlockStatements", "}")), _, children) =>
        return new Block(Some(unfoldStatements(children(1))))
      case TreeNode(ProductionRule("Block", Seq("{", "}")), _, children) =>
        return new Block(None)
      case _ => throw new AstConstructionException(
        "Invalid tree node to create Block"
      )
    }
  }
}
