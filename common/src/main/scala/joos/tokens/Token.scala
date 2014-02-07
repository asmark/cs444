package joos.tokens

import joos.tokens.TokenKind.TokenKind

abstract class Token {
  def symbol: String

  def lexeme: String
}

case class TerminalToken(val lexeme: String, val kind: TokenKind) extends Token {
  override def symbol = TokenKind.kindToSymbol(kind)
}

case class NonTerminalToken(val lexeme: String, val symbol: String) extends Token