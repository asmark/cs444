package joos.tokens

import joos.tokens.TokenKind.TokenKind

case class Token(val kind: TokenKind, val lexeme: String)
