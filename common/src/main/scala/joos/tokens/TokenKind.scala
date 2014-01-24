package joos.tokens

object TokenKind extends Enumeration {
  def getHighestPriority(kinds: Set[TokenKind]): TokenKind = {
    return values.find(kinds.contains(_)).get
  }

  type TokenKind = Value
  // Same order as TokenKindRegexp


  // Keywords
  final val Final = Value("Final")

  // Literals
  final val DecimalInteger = Value("DecimalInteger")

  final val HexInteger = Value("HexInteger")

  final val OctalInteger = Value("OctalInteger")

  final val Float = Value("Float")

  final val True = Value("True")

  final val False = Value("False")

  final val Character = Value("Character")

  final val Null = Value("Null")

  // Separators
  final val LeftParen = Value("LeftParen")

  final val RightParen = Value("RightParen")

  final val LeftBrace = Value("LeftBrace")

  final val RightBrace = Value("RightBrace")

  final val LeftBracket = Value("LeftBracket")

  final val RightBracket = Value("RightBracket")

  final val SemiColon = Value("SemiColon")

  final val Comma = Value("Comma")

  final val Dot = Value("Dot")

  // Operators
  final val Assign = Value("=")

  final val Greater = Value("Greater")

  // ...

  // Identifier
  final val Id = Value("Id")

}
