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
  final val TraditionalCommentPrefix = {
    Concatenation("/*")
  }

  final val TraditionalCommentPostfix = {
    Concatenation("*/")
  }

  final val EOLCommentPrefix = {
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
  final val Final = {
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
    val first_form =
      Concatenation(DIGITS) +
      Atom('.') +
      Optional(Concatenation(DIGITS)) +
      Optional(exponentPart()) +
      Optional(floatTypeSuffix())

    val second_form =
      Atom('.') +
      Concatenation(DIGITS) +
      Optional(exponentPart()) +
      Optional(floatTypeSuffix())

    val third_form =
      Concatenation(DIGITS) +
      exponentPart() +
      Optional(floatTypeSuffix())

    val fourth_form =
      Concatenation(DIGITS) +
      Optional(exponentPart()) +
      floatTypeSuffix()

    (first_form | second_form | third_form | fourth_form)
  }

  // BooleanLiteral
  // TODO
  final val TRUE = {
    Concatenation("true")
  }

  final val FALSE = {
    Concatenation("false")
  }

  // CharacterLiteral
  // Regular Expression:
  final val CharacterLiteral = {
    Atom('\'') + Alternation(SingleCharacter.mkString("")) + Atom('\'') |
      Atom('\'') + escapeSequence() + Atom('\'')
  }

  // StringLiteral
  // Regular Expression: "StringCharacter*" | "", StringCharacter -> InputCharacter but not " or \ | EscapeSequence
  final val StringLiteral = {
    Atom('\"') + (stringCharacter()*) + Atom('\"')
  }

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
  final val SemiComma = {
    Atom(';')
  }

  // ,
  final val Comma = {
    Atom(',')
  }

  // .
  final val Dot = {
    Atom('.')
  }

  // Operators
  // TODO: 37 operators in total
  final val Assign = {
    Atom('=')
  }

  final val Greater = {
    Atom('>')
  }

  final val Less = {
    Atom('<')
  }

  final val Exclamation = {
    Atom('!')
  }

  final val Period = {
    Atom('~')
  }

  final val Question = {
    Atom('?')
  }

  final val Colon = {
    Atom(':')
  }

  final val Equal = {
    Concatenation("==")
  }

  final val LessEqual = {
    Concatenation("<=")
  }

  final val GreaterEqual = {
    Concatenation("<=")
  }

  final val NotEqual = {
    Concatenation("!=")
  }

  final val And = {
    Concatenation("&&")
  }

  final val Or = {
    Concatenation("||")
  }

  final val Increment = {
    Concatenation("++")
  }

  final val Decrement = {
    Concatenation("++")
  }

  final val Plus = {
    Atom('+')
  }

  final val Minus = {
    Atom('-')
  }

  final val Multiply = {
    Atom('*')
  }

  final val DIVIDE = {
    Atom('/')
  }

  final val BitAnd = {
    Atom('&')
  }

  final val BitOr = {
    Atom('|')
  }

  final val Carrot = {
    Atom('^')
  }

  final val Modulo = {
    Atom('%')
  }

  final val LeftShift = {
    Concatenation("<<")
  }

  final val RightShift = {
    Concatenation(">>")
  }

  final val UnsignedShift = {
    Concatenation(">>>")
  }

  final val PlusAssign = {
    Concatenation("+=")
  }

  final val MinusAssign = {
    Concatenation("-=")
  }

  final val MultiplyAssign = {
    Concatenation("*=")
  }

  final val DivideAssign = {
    Concatenation("/=")
  }

  final val BitAndAssign = {
    Concatenation("&=")
  }

  final val BitOrAssign = {
    Concatenation("|=")
  }

  final val CarrotAssign = {
    Concatenation("^=")
  }

  final val ModuloAssign = {
    Concatenation("%=")
  }

  final val LeftShiftAssign = {
    Concatenation("<<=")
  }

  final val RightShiftAssign = {
    Concatenation(">>=")
  }

  final val UsignedRightShiftAssign = {
    Concatenation(">>>=")
  }
}