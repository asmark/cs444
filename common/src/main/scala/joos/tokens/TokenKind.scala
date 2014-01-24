package joos.tokens

import joos.regexp.RegularExpression

object TokenKind extends Enumeration {
  type TokenKind = Value

  case class TokenKindValue(val name: String, regexp: RegularExpression) extends Val(name) {
    def getRegexp() = regexp := this
  }

  def getHighestPriority(kinds: Set[TokenKind]): TokenKind = {
    return values.find(kind => kinds.contains(kind)).get
  }

  // In order of priority

  // Keywords
  final val Final = TokenKindValue("Final", TokenKindRegexp.Final)


  // Literals
  final val DecimalInteger = TokenKindValue("DecimalInteger", TokenKindRegexp.DecimalIntLiteral)

  final val HexInteger = TokenKindValue("HexInteger", TokenKindRegexp.HexIntLiteral)

  final val OctalInteger = TokenKindValue("OctalInteger", TokenKindRegexp.OctalIntLiteral)

  final val Float = TokenKindValue("Float", TokenKindRegexp.FloatLiteral)

  final val True = TokenKindValue("True", TokenKindRegexp.True)

  final val False = TokenKindValue("False", TokenKindRegexp.False)

  final val Character = TokenKindValue("Character", TokenKindRegexp.CharacterLiteral)

  final val Null = TokenKindValue("Null", TokenKindRegexp.Null)

  // Separators
  final val LeftParen = TokenKindValue("LeftParen", TokenKindRegexp.LeftParen)

  final val RightParen = TokenKindValue("RightParen", TokenKindRegexp.RightParen)

  final val LeftBrace = TokenKindValue("LeftBrace", TokenKindRegexp.LeftBrace)

  final val RightBrace = TokenKindValue("RightBrace", TokenKindRegexp.RightBrace)

  final val LeftBracket = TokenKindValue("LeftBracket", TokenKindRegexp.LeftBracket)

  final val RightBracket = TokenKindValue("RightBracket", TokenKindRegexp.RightBracket)

  final val SemiColon = TokenKindValue("SemiColon", TokenKindRegexp.SemiColon)

  final val Comma = TokenKindValue("Comma", TokenKindRegexp.Comma)

  final val Dot = TokenKindValue("Dot", TokenKindRegexp.Dot)

  // Operators
  final val Assign = TokenKindValue("=", TokenKindRegexp.Assign)

  final val Greater = TokenKindValue("Greater", TokenKindRegexp.Greater)

  // ...

  // Identifier
  final val Id = TokenKindValue("Id", TokenKindRegexp.Id)

}
