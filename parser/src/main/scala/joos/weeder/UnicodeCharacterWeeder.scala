package joos.weeder

import java.util.regex.Pattern
import joos.parser.ParseMetaData
import joos.parsetree.{LeafNode, ParseTreeNode}
import joos.tokens.{TokenKind, TerminalToken}

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
