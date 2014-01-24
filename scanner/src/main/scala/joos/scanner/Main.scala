package joos.scanner

import joos.regexp.{Atom, RegularExpression}
import joos.tokens.TokenKind
import joos.tokens.TokenKind.TokenKindValue

object Main {

  def main(args: Array[String]) {
    // Example of how to append all tokens into one DFA
    val joosRegexp = TokenKind.values.map(_.asInstanceOf[TokenKindValue].getRegexp()).reduceRight((a,b) => a | b)
    val scanner = Scanner.forRegexp(joosRegexp)

    "abcdefg".foreach(scanner.parse(_))
    scanner.getTokens().foreach(println(_))
  }

}
