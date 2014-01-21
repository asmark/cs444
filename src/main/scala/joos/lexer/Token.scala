package joos.lexer

object Token {
  def generateStaticWord(word:String):RegularExpression = {
    val symbols = word.toCharArray
    val atoms = new Array[RegularExpression](symbols.length)
    for (i <- 0 to symbols.length - 1) {
      if (i != symbols.length - 1) {
        atoms(i) = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), symbols(i))
      } else {
        atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode(word), symbols(i))
      }
    }
    RegularExpression.concatAll(atoms)
  }

  // Comments
  // TODO

  // Identifier
  // TODO

  // Keywords

  final val FINAL = {
    generateStaticWord("final")
  }

  // Literals

  // IntegerLiteral

  // DecimalIntegerLiteral
  // Regular Expression: ([1-9][0-9]*|[1-9][0-9]*(l|L))
  final val INTEGER_LITERAL = {
      val digit_symbols = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
      val digit_atoms = new Array[RegularExpression](digit_symbols.length)
      for (i <- 0 until digit_symbols.length) {
        digit_atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), digit_symbols(i))
      }

      val lower_l = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), 'l')
      var upper_l = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), 'L')

      val first_digit_atoms = digit_atoms.slice(1, digit_atoms.length)
      val non_postfix =
        RegularExpression.concatAll(
          Array(
            RegularExpression.alterAll(first_digit_atoms),
            (RegularExpression.alterAll(digit_atoms))*
          )
        )
      val postfix =
        RegularExpression.concatAll(
          Array(
            RegularExpression.concatAll(
              Array(
                RegularExpression.alterAll(first_digit_atoms),
                (RegularExpression.alterAll(digit_atoms))*
              )
            ),
            RegularExpression.alterAll(Array(lower_l, upper_l))
          )
        )

      RegularExpression.alterAll(Array(non_postfix, postfix))
    }

  // HexNumeral
  // Regular Expression: 0x[1-9a-fA-F][1-9a-fA-F]*
  final val HEX_NUMERAL = {
    val digit_symbols = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f',
      'A', 'B', 'C', 'D', 'E', 'F')
    val digit_atoms = new Array[RegularExpression](digit_symbols.length)
    for (i <- 0 until digit_symbols.length) {
      digit_atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), digit_symbols(i))
    }

    val prefix = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), '0') +
      new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), 'x')

    RegularExpression.concatAll(
      Array(
        prefix,
        RegularExpression.concatAll(digit_atoms),
        (RegularExpression.concatAll(digit_atoms))*
      )
    )
  }

  // HexNumeral
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
  // TODO: 37 operators
}
