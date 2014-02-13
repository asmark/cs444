package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.TerminalToken
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class PrimitiveType(primType: TerminalToken) extends Type

object PrimitiveType {
  def extractNumericToken(numericType: ParseTreeNode): TerminalToken = {
    numericType.children(0).children(0).token.asInstanceOf[TerminalToken]
  }

  def apply(ptn: ParseTreeNode): PrimitiveType = {
    ptn match {
      case TreeNode(ProductionRule("PrimitiveType", Seq("NumericType")), _, children) =>
        return new PrimitiveType(extractNumericToken(children(0)))
      case TreeNode(ProductionRule("PrimitiveType", Seq("boolean")), _, children) =>
        return new PrimitiveType(children(0).token.asInstanceOf[TerminalToken])
      case _ => throw new AstConstructionException(
        "Invalid tree node to create PrimitiveType"
      )
    }
  }
}