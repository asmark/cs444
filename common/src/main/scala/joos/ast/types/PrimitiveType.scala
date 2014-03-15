package joos.ast.types

import joos.ast.AstConstructionException
import joos.ast.declarations.TypeDeclaration
import joos.core.Enumeration
import joos.syntax.language.ProductionRule
import joos.syntax.parsetree.{TreeNode, ParseTreeNode}
import joos.syntax.tokens.TerminalToken

class PrimitiveType(val name: String) extends Type with PrimitiveType.Value {
  override var declaration: Option[TypeDeclaration] = None
  override def standardName = name
}

object PrimitiveType extends Enumeration {
  type T = PrimitiveType

  final val IntegerType = this + new PrimitiveType("int")
  final val CharType = this + new PrimitiveType("char")
  final val BooleanType = this + new PrimitiveType("boolean")
  final val ByteType = this + new PrimitiveType("byte")
  final val ShortType = this + new PrimitiveType("short")
  final val VoidType = this + new PrimitiveType("void")
  final val NullType = this + new PrimitiveType("null")

  private[this] def extractNumericToken(numericType: ParseTreeNode) = {
    numericType.children(0).children(0).token match {
      case terminalToken: TerminalToken => fromName(terminalToken.lexeme)
      case _ => throw new AstConstructionException(
        "Invalid tree node to create NumericType"
      )
    }
  }

  def apply(ptn: ParseTreeNode): PrimitiveType = {
    ptn match {
      case TreeNode(ProductionRule("PrimitiveType", Seq("NumericType")), _, children) =>
        extractNumericToken(children(0))
      case TreeNode(ProductionRule("PrimitiveType", Seq("boolean")), _, children) =>
        children(0).token match {
          case terminalToken: TerminalToken => BooleanType
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
