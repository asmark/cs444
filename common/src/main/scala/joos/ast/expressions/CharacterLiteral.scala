package joos.ast.expressions

import joos.ast.AstConstructionException
import joos.ast.types.PrimitiveType
import joos.syntax.parsetree.{LeafNode, ParseTreeNode}
import joos.syntax.tokens.{TokenKind, TerminalToken}
import org.apache.commons.lang.StringEscapeUtils

case class CharacterLiteral(token: TerminalToken) extends LiteralExpression {
  expressionType = PrimitiveType.CharType

  override def toString = token.lexeme

  val value = {
    if (token.lexeme.length == 3) token.lexeme.charAt(1).toInt
    else {
      token.lexeme.toList.drop(1).dropRight(1) match {
          // Simple escape. Ie '\n' => 10
        case List('\\', x) => StringEscapeUtils.unescapeJava(s"\\${x}").toCharArray.head.toInt
          // Octal escape. Ie. '\053' => 53
        case '\\' :: x => StringEscapeUtils.unescapeJava(x.mkString).toInt
      }
    }
  }
}

object CharacterLiteral {
  def apply(ptn: ParseTreeNode): CharacterLiteral = {
    ptn match {
      case LeafNode(token) if token.kind == TokenKind.CharacterLiteral =>
        CharacterLiteral(token)
      case _ => throw new AstConstructionException("No valid production rule to make CharacterLiteral")
    }
  }
}
