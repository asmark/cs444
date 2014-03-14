package joos.ast.types

import joos.ast.AstConstructionException
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}

case class PrimitiveType(token: TerminalToken) extends Type

object PrimitiveType {
  val IntegerType = PrimitiveType(TerminalToken("int", TokenKind.Int))
  val CharType = PrimitiveType(TerminalToken("char", TokenKind.Char))
  val BooleanType = PrimitiveType(TerminalToken("boolean", TokenKind.Boolean))
  val ByteType = PrimitiveType(TerminalToken("byte", TokenKind.Byte))
  val ShortType = PrimitiveType(TerminalToken("short", TokenKind.Short))
  val VoidType = PrimitiveType(TerminalToken("void", TokenKind.Void))

  def isNumeric(inputType: Type): Boolean = {
    inputType match {
      case IntegerType | ByteType | CharType | ShortType=> true
      case _ => false
    }
  }

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
