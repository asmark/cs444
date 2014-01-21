package joos.lexer.TokenKind

import joos.lexer._
import joos.lexer.MultiAlter
import joos.lexer.Atom
import joos.lexer.NonAcceptingNFANode
import joos.lexer.MultiConcat
import joos.lexer.AcceptingNFANode

object TokenKinds {
  // Comments
  // TODO

  // Identifier
  // Regular Expression: [JavaLetter][JavaLetter or Digits]*
  final val ID = {
    val java_letters = symbolsToAtoms(alphabets)
    val java_letters_or_number = java_letters ++ symbolsToAtoms(digits)
    new MultiAlter(java_letters) + (new MultiAlter(java_letters_or_number)*)
  }

  // Keywords
  final val FINAL = {
    generateStaticWord("final")
  }

  // Literals

  // IntegerLiteral

  // DecimalIntegerLiteral
  // Regular Expression: ([1-9][0-9]*|[1-9][0-9]*(l|L))
  final val INTEGER_LITERAL = {
    val digit_symbols = digits.clone()
    val digit_atoms = new Array[RegularExpression](digit_symbols.length)
    for (i <- 0 until digit_symbols.length) {
      digit_atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), digit_symbols(i))
    }

    val first_digit_atoms = digit_atoms.slice(1, digit_atoms.length)
    val non_postfix =
      new MultiAlter(first_digit_atoms) +
        ((new MultiAlter(digit_atoms))*)

    val postfix =
      new MultiAlter(first_digit_atoms) +
        (new MultiAlter(digit_atoms)*) +
        (lower_l | upper_l)

    non_postfix | postfix
  }

  // HexNumeral
  // Regular Expression: (0x[1-9a-fA-F][1-9a-fA-F]*|0x[1-9a-fA-F][1-9a-fA-F]*(l|L))
  final val HEX_NUMERAL = {
    val digit_symbols = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
      'A', 'B', 'C', 'D', 'E', 'F')
    val digit_atoms = new Array[RegularExpression](digit_symbols.length)
    for (i <- 0 until digit_symbols.length) {
      digit_atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), digit_symbols(i))
    }

    val prefix = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), '0') +
      new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), 'x')

    val non_postfix = prefix + new MultiConcat(digit_atoms) + (new MultiConcat(digit_atoms)*)
    val postfix = non_postfix + (lower_l | upper_l)
    (non_postfix | postfix)
  }

  // OctalIntegerLiteral
  // Regular Expression: 0[0-7][0-7]*
  // TODO

  // FloatingPointLiteral
  // TODO

  // BooleanLiteral
  // TODO

  // StringLiteral
  // TODO

  // Null Literal
  final val NULL = {
    generateStaticWord("null")
  }

  // Separators

  // (
  final val LEFT_PAREN = {
    generateStaticWord("(")
  }

  // )
  final val RIGHT_PAREN = {
    generateStaticWord(")")
  }

  // {
  final val LEFT_BRACE = {
    generateStaticWord("{")
  }

  // }
  final val RIGHT_BRACE = {
    generateStaticWord("}")
  }

  // [
  final val LEFT_BRACKET = {
    generateStaticWord("[")
  }

  // ]
  final val RIGHT_BRACKET = {
    generateStaticWord("]")
  }

  // ;
  // TODO

  // ,
  // TODO

  // .
  // TODO

  // Operators
  // TODO: 37 operators in total
  final val EQUAL = {
    generateStaticWord("=")
  }

  final val GT = {
    generateStaticWord(">")
  }

  final val LT = {
    generateStaticWord("<")
  }
}
