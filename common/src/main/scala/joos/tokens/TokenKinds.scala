package joos.tokens

import joos._
import scala.language.postfixOps

object TokenKinds {
  // Comments
  // TODO: Adjust this part when we get to parsing
  final val TraditionalCommentPrefix = {
    import joos.regexp.Concatenation
    Concatenation("/*")
  }

  final val TraditionalCommentPostfix = {
    import joos.regexp.Concatenation
    Concatenation("*/")
  }

  final val EolCommentPrefix = {
    import joos.regexp.Concatenation
    Concatenation("//")
  }

  // Identifier
  // Regular Expression: [JavaLetter][JavaLetter or Digits]*
  final val ID = {
    import joos.automata.{AcceptingNfaNode, NonAcceptingNfaNode, NfaNode}
    import joos.regexp.{Alternation, Atom}
    Alternation(JAVA_LETTERS) +
      (Alternation(JAVA_LETTERS + DIGITS) *) +
      Atom(NonAcceptingNfaNode(), AcceptingNfaNode("ID"), NfaNode.Epsilon)
  }

  // Keywords
  final val Final = {
    import joos.automata.{AcceptingNfaNode, NonAcceptingNfaNode, NfaNode}
    import joos.regexp.{Concatenation, Atom}
    Concatenation("final") + Atom(NonAcceptingNfaNode(), AcceptingNfaNode("final"), NfaNode.Epsilon)
  }


  // Literals

  // IntegerLiteral

  // DecimalIntegerLiteral
  // Regular Expression: ([1-9][0-9]*|[1-9][0-9]*(l|L))
  final val DecimalIntLiteral = {
    import joos.regexp.{Alternation, Atom}
    val non_postfix = Alternation(NON_ZERO_DIGITS) + (Alternation(DIGITS) *)
    val postfix = Alternation(NON_ZERO_DIGITS) + (Alternation(DIGITS) *) + (Atom('l') | Atom('L'))
    non_postfix | postfix
  }

  // HexNumeral
  // Regular Expression: (0x[0-9a-fA-F][0-9a-fA-F]*|0x[0-9a-fA-F][0-9a-fA-F]*(l|L))
  final val HexIntLiteral = {
    import joos.regexp.{Alternation, Concatenation, Atom}
    val prefix = Atom('0') + Atom('x')

    val non_postfix = prefix + Alternation(HEX_DIGITS) + (Alternation(HEX_DIGITS) *)
    val postfix =
      Concatenation(
        Seq(
          prefix,
          Alternation(HEX_DIGITS),
          (Alternation(HEX_DIGITS) *),
          Atom('l') | Atom('L')
        )
      )

    (non_postfix | postfix)
  }

  // OctalIntegerLiteral
  // Regular Expression: 0[0-7][0-7]*
  final val OctalIntLiteral = {
    import joos.regexp.{Alternation, Concatenation, Atom}
    Concatenation(Seq(Atom('0'), Alternation(OCTAL_DIGITS), (Alternation(OCTAL_DIGITS)) *)) |
      Concatenation(Seq(Atom('0'), Alternation(OCTAL_DIGITS), (Alternation(OCTAL_DIGITS)) *, Atom('l') | Atom('L')))
  }


  // FloatingPointLiteral
  // Regular Expression: TOO LONG
  final val FloatLiteral = {
    import joos.regexp.{Concatenation, Atom}
    val first_form =
      Concatenation(DIGITS) +
        Atom('.') +
        ~Concatenation(DIGITS) +
        ~exponentPart() +
        ~floatTypeSuffix()

    val second_form =
      Atom('.') +
        Concatenation(DIGITS) +
        ~exponentPart() +
        ~floatTypeSuffix()

    val third_form =
      Concatenation(DIGITS) +
        exponentPart() +
        ~floatTypeSuffix()

    val fourth_form =
      Concatenation(DIGITS) +
        ~exponentPart() +
        floatTypeSuffix()

    first_form | second_form | third_form | fourth_form
  }

  // BooleanLiteral
  // TODO
  final val True = {
    import joos.regexp.Concatenation
    Concatenation("true")
  }

  final val False = {
    import joos.regexp.Concatenation
    Concatenation("false")
  }

  // CharacterLiteral
  // Regular Expression:
  final val CharacterLiteral = {
    import joos.regexp.{Alternation, Atom}
    Atom('\'') + Alternation(SingleCharacter.mkString("")) + Atom('\'') |
      Atom('\'') + escapeSequence() + Atom('\'')
  }

  // StringLiteral
  // Regular Expression: "StringCharacter*" | "", StringCharacter -> InputCharacter but not " or \ | EscapeSequence
  final val StringLiteral = {
    import joos.regexp.Atom
    Atom('\"') + (stringCharacter() *) + Atom('\"')
  }

  // Null Literal
  final val Null = {
    import joos.regexp.Concatenation
    Concatenation("null")
  }

  // Separators

  // (
  final val LeftParen = {
    import joos.regexp.Atom
    Atom('(')
  }

  // )
  final val RightParen = {
    import joos.regexp.Atom
    Atom(')')
  }

  // {
  final val LeftBrace = {
    import joos.regexp.Atom
    Atom('{')
  }

  // }
  final val RightBrace = {
    import joos.regexp.Atom
    Atom('}')
  }

  // [
  final val LeftBracket = {
    import joos.regexp.Atom
    Atom('[')
  }

  // ]
  final val RightBracket = {
    import joos.regexp.Atom
    Atom(']')
  }

  // ;
  final val SemiColon = {
    import joos.regexp.Atom
    Atom(';')
  }

  // ,
  final val Comma = {
    import joos.regexp.Atom
    Atom(',')
  }

  // .
  final val Dot = {
    import joos.regexp.Atom
    Atom('.')
  }

  // Operators
  // TODO: 37 operators in total
  final val Assign = {
    import joos.regexp.Atom
    Atom('=')
  }

  final val Greater = {
    import joos.regexp.Atom
    Atom('>')
  }

  final val Less = {
    import joos.regexp.Atom
    Atom('<')
  }

  final val Exclamation = {
    import joos.regexp.Atom
    Atom('!')
  }

  final val Period = {
    import joos.regexp.Atom
    Atom('~')
  }

  final val Question = {
    import joos.regexp.Atom
    Atom('?')
  }

  final val Colon = {
    import joos.regexp.Atom
    Atom(':')
  }

  final val Equal = {
    import joos.regexp.Concatenation
    Concatenation("==")
  }

  final val LessEqual = {
    import joos.regexp.Concatenation
    Concatenation("<=")
  }

  final val GreaterEqual = {
    import joos.regexp.Concatenation
    Concatenation("<=")
  }

  final val NotEqual = {
    import joos.regexp.Concatenation
    Concatenation("!=")
  }

  final val And = {
    import joos.regexp.Concatenation
    Concatenation("&&")
  }

  final val Or = {
    import joos.regexp.Concatenation
    Concatenation("||")
  }

  final val Increment = {
    import joos.regexp.Concatenation
    Concatenation("++")
  }

  final val Decrement = {
    import joos.regexp.Concatenation
    Concatenation("++")
  }

  final val Plus = {
    import joos.regexp.Atom
    Atom('+')
  }

  final val Minus = {
    import joos.regexp.Atom
    Atom('-')
  }

  final val Multiply = {
    import joos.regexp.Atom
    Atom('*')
  }

  final val DIVIDE = {
    import joos.regexp.Atom
    Atom('/')
  }

  final val BitAnd = {
    import joos.regexp.Atom
    Atom('&')
  }

  final val BitOr = {
    import joos.regexp.Atom
    Atom('|')
  }

  final val Carrot = {
    import joos.regexp.Atom
    Atom('^')
  }

  final val Modulo = {
    import joos.regexp.Atom
    Atom('%')
  }

  final val LeftShift = {
    import joos.regexp.Concatenation
    Concatenation("<<")
  }

  final val RightShift = {
    import joos.regexp.Concatenation
    Concatenation(">>")
  }

  final val UnsignedShift = {
    import joos.regexp.Concatenation
    Concatenation(">>>")
  }

  final val PlusAssign = {
    import joos.regexp.Concatenation
    Concatenation("+=")
  }

  final val MinusAssign = {
    import joos.regexp.Concatenation
    Concatenation("-=")
  }

  final val MultiplyAssign = {
    import joos.regexp.Concatenation
    Concatenation("*=")
  }

  final val DivideAssign = {
    import joos.regexp.Concatenation
    Concatenation("/=")
  }

  final val BitAndAssign = {
    import joos.regexp.Concatenation
    Concatenation("&=")
  }

  final val BitOrAssign = {
    import joos.regexp.Concatenation
    Concatenation("|=")
  }

  final val CarrotAssign = {
    import joos.regexp.Concatenation
    Concatenation("^=")
  }

  final val ModuloAssign = {
    import joos.regexp.Concatenation
    Concatenation("%=")
  }

  final val LeftShiftAssign = {
    import joos.regexp.Concatenation
    Concatenation("<<=")
  }

  final val RightShiftAssign = {
    import joos.regexp.Concatenation
    Concatenation(">>=")
  }

  final val UsignedRightShiftAssign = {
    import joos.regexp.Concatenation
    Concatenation(">>>=")
  }
}
