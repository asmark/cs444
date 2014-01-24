package joos.lexer

package object TokenKind {
  def generateStaticWord(word: String): RegularExpression = {
    val symbols = word.toCharArray
    val atoms = symbolsToAtoms(symbols)
    new Concatenation(atoms) + new Atom(NonAcceptingNfaNode(), AcceptingNfaNode(word), NfaNode.Epsilon)
  }

  def symbolsToAtoms(symbols: Array[Char]): Array[RegularExpression] = {
    val atoms = new Array[RegularExpression](symbols.length)
    for (i <- 0 to atoms.length - 1) {
      atoms(i) = new Atom(NonAcceptingNfaNode(), NonAcceptingNfaNode(), symbols(i))
    }
    atoms
  }

  // The postfix for integer literals
  final val LOWER_L = new Atom(NonAcceptingNfaNode(), AcceptingNfaNode(), 'l')
  final val UPPER_L = new Atom(NonAcceptingNfaNode(), AcceptingNfaNode(), 'L')

  final val DIGITS = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9').mkString("")
  final val NON_ZERO_DIGITS = DIGITS.slice(1, DIGITS.length - 1).mkString("")
  final val HEX_DIGITS = (DIGITS ++ Array('a', 'b', 'c', 'd', 'e', 'f',
    'A', 'B', 'C', 'D', 'E', 'F')).mkString("")
  final val OCTAL_DIGITS = DIGITS.slice(0, 7).mkString("")

  final val ALPHABETS = {
    val lower = "abcdefghijklmnopqrstuvwxyz"
    val upper = lower.toUpperCase
    lower + upper
  }

  final val JAVA_LETTERS = {
    ALPHABETS + '_' + '$'
  }


}
