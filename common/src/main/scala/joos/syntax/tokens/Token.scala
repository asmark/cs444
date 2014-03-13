package joos.syntax.tokens

import joos.syntax.tokens.TokenKind.{TokenKind}

abstract class Token {
  def symbol: String

  def lexeme: String
}

case class TerminalToken(lexeme: String, kind: TokenKind) extends Token {
  override def symbol = if (kind != null) TokenKind.kindToSymbol(kind) else lexeme
}

case class NonTerminalToken(lexeme: String, symbol: String) extends Token