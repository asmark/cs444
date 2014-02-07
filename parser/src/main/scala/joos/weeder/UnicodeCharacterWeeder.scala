package joos.weeder

import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.parser.ParseMetaData
import joos.tokens.{TokenKind, TerminalToken}
import java.util.regex.Pattern
import joos.weeder.exceptions.WeederException

case class UnicodeCharacterWeeder() extends Weeder{
  lazy final val UnicodePattern = Pattern.compile("""\\u[a-fA-F0-9]{4}""")
  lazy final val OctalPattern = Pattern.compile("""[^\\]\\([^0-7btnfr"'\\])""")
  override def check(ptn: ParseTreeNode, md: ParseMetaData): Unit = {
    ptn match {
      case LeafNode(TerminalToken(lexeme, TokenKind.StringLiteral)) => {
        if (UnicodePattern.matcher(lexeme).find() || OctalPattern.matcher(lexeme).find()) {
          throw new WeederException("Unicode/Octal escapes in string literals")
        }
      }
      case LeafNode(TerminalToken(lexeme, TokenKind.CharacterLiteral)) => {
        if (UnicodePattern.matcher(lexeme).find() || OctalPattern.matcher(lexeme).find()) {
          throw new WeederException("Unicode/Octal escapes in character literals")
        }
      }
      case _ => {}
    }
  }
}
