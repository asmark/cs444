package joos.ast.statements

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import scala.collection.mutable.ArrayBuffer

case class Block(statements: IndexedSeq[Statement]) extends Statement {
  override def toString = s"{\n${statements.mkString(";\n")}}\n"
}

object Block {
  private[this] def unfoldStatements(blockStatements: ParseTreeNode): ArrayBuffer[Statement] = {
    blockStatements match {
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatement")), _, children) =>
        ArrayBuffer(Statement(children(0).children(0)))
      case TreeNode(ProductionRule("BlockStatements", Seq("BlockStatements", "BlockStatement")), _, children) =>
        unfoldStatements(children(0)) += Statement(children(1).children(0))
      case _ => throw new AstConstructionException("Invalid tree node to create BlockStatements")
    }
  }

  def apply(ptn: ParseTreeNode): Block = {
    ptn match {
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "BlockStatements", "}")), _, children) =>
        Block(unfoldStatements(children(1)))
      case TreeNode(ProductionRule("Block" | "ConstructorBody", Seq("{", "}")), _, children) =>
        Block(IndexedSeq())
      case _ => throw new AstConstructionException("Invalid tree node to create Block")
    }
  }
}
