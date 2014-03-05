package joos.ast.types

import joos.ast.AstConstructionException
import joos.language.ProductionRule
import joos.parsetree.{TreeNode, ParseTreeNode}
import joos.tokens.TerminalToken

case class PrimitiveType(token: TerminalToken) extends Type

object PrimitiveType {

  final val Types = Set(
    "int",
    "long",
    "byte",
    "boolean",
    "char",
    "double",
    "float",
    "short"
  )

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
          case terminalToken: TerminalToken => PrimitiveType(terminalToken)
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
