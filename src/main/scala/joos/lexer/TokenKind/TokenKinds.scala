package joos.lexer.TokenKind

import joos.lexer._
import joos.lexer.Alternation
import joos.lexer.Atom
import joos.lexer.NonAcceptingNfaNode
import joos.lexer.Concatenation
import joos.lexer.AcceptingNfaNode
import scala.language.postfixOps

object TokenKinds {
  // Comments
  // TODO: Adjust this part when we get to parsing
  final val TRADITIONAL_COMMENT_PREFIX = {
    Concatenation("/*")
  }

  final val TRADITIONAL_COMMENT_POSTFIX = {
    Concatenation("*/")
  }

  final val EOL_COMMENT_START = {
    Concatenation("//")
  }

  // Identifier
  // Regular Expression: [JavaLetter][JavaLetter or Digits]*
  final val ID = {
    Alternation(JAVA_LETTERS) +
      (Alternation(JAVA_LETTERS + DIGITS)*) +
      Atom(NonAcceptingNfaNode(), AcceptingNfaNode("ID"), NfaNode.Epsilon)
  }

  // Keywords
  final val FINAL = {
    Concatenation("final")
  }

  // Literals

  // IntegerLiteral

  // DecimalIntegerLiteral
  // Regular Expression: ([1-9][0-9]*|[1-9][0-9]*(l|L))
  final val DECIMAL_INT_LITERAL = {
    val non_postfix = Alternation(NON_ZERO_DIGITS) + (Alternation(DIGITS)*)
    val postfix = Alternation(NON_ZERO_DIGITS) + (Alternation(DIGITS)*) + (LOWER_L | UPPER_L)
    non_postfix | postfix
  }

  // HexNumeral
  // Regular Expression: (0x[0-9a-fA-F][0-9a-fA-F]*|0x[0-9a-fA-F][0-9a-fA-F]*(l|L))
  final val HEX_INT_LITERAL = {
    val prefix = Atom('0') + Atom('x')

    val non_postfix = prefix + Alternation(HEX_DIGITS) + (Alternation(HEX_DIGITS)*)
    val postfix =
      Concatenation(
        Seq(
          prefix,
          Alternation(HEX_DIGITS),
          (Alternation(HEX_DIGITS)*)
        )
      )

    (non_postfix | postfix)
  }

  // OctalIntegerLiteral
  // Regular Expression: 0[0-7][0-7]*
  final val OCTAL_INT_LITERAL = {
    Concatenation(Seq(Atom('0'), Alternation(OCTAL_DIGITS), (Alternation(OCTAL_DIGITS))*))
  }

  // FloatingPointLiteral
  // TODO: TOO LONG
  final val FLOAT_LITERAL = {
    val digit_
  }

  // BooleanLiteral
  // TODO

  // StringLiteral
  // TODO

  // Test literal
  final val TEST = {
    (Atom('T') | Atom('t')) + Atom('e') + Atom('s') + Atom('t') + Atom(NonAcceptingNfaNode(), AcceptingNfaNode("test"), NfaNode.Epsilon)
  }

  // Null Literal
  final val NULL = {
    Concatenation("null")
  }

  // Separators

  // (
  final val LEFT_PAREN = {
    Atom('(')
  }

  // )
  final val RIGHT_PAREN = {
    Atom(')')
  }

  // {
  final val LEFT_BRACE = {
    Atom('{')
  }

  // }
  final val RIGHT_BRACE = {
    Atom('}')
  }

  // [
  final val LEFT_BRACKET = {
    Atom('[')
  }

  // ]
  final val RIGHT_BRACKET = {
    Atom(']')
  }

  // ;
  final val SEMI_COMMA = {
    Atom(';')
  }

  // ,
  // TODO
  final val COMMA = {
    Atom(',')
  }

  // .
  // TODO
  final val PERIOD = {
    Atom('.')
  }

  // Operators
  // TODO: 37 operators in total
  final val EQUAL = {
    Atom('=')
  }

  final val GT = {
    Atom('>')
  }

  final val LT = {
    Atom('<')
  }
}
