package joos.ast

import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.TerminalToken
import joos.language.ProductionRule
import joos.ast.exceptions.AstConstructionException

case class PrimitiveType(primType: TerminalToken) extends Type

object PrimitiveType {
  private def extractNumericToken(numericType: ParseTreeNode): TerminalToken = {
    numericType.children(0).children(0).token match {
      case terminalToken: TerminalToken => terminalToken
      case _ => throw new AstConstructionException(
        "Invalid tree node to create NumericType"
      )
    }
  }

  def apply(ptn: ParseTreeNode): PrimitiveType = {
    ptn match {
      case TreeNode(ProductionRule("PrimitiveType", Seq("NumericType")), _, children) =>
        PrimitiveType(extractNumericToken(children(0)))
      case TreeNode(ProductionRule("PrimitiveType", Seq("boolean")), _, children) =>
        children(0).token match {
          case terminalToken: TerminalToken => return PrimitiveType(terminalToken)
          case _ => throw new AstConstructionException(
            "Invalid tree node to create boolean"
          )
        }
      case _ => throw new AstConstructionException(
        "Invalid tree node to create PrimitiveType"
      )
    }
  }
}