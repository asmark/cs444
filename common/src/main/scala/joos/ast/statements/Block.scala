package joos.ast.statements

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.semantic.BlockEnvironment

case class Block(statements: Seq[Statement]) extends Statement {
  override def toString = s"{\n${statements.mkString(";\n")}}\n"
}

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
