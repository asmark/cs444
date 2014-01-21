package joos.lexer

package object TokenKind {
  def generateStaticWord(word:String):RegularExpression = {
    val symbols = word.toCharArray
    val atoms = symbolsToAtoms(symbols)
    atoms.last.exit = AcceptingNFANode(word)
    new MultiConcat(atoms)
  }

  def symbolsToAtoms(symbols:Array[Char]): Array[RegularExpression] = {
    val atoms = new Array[RegularExpression](symbols.length)
    for (i <- 0 to atoms.length) {
      atoms(i) = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), symbols(i))
    }
    atoms
  }

  // The postfix for integer literals
  final val lower_l = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), 'l')
  final val upper_l = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), 'L')

  final val digits = Array('0', '1', '2', '3', '4', '5', '6', '7', '8', '9')
  final val alphabets = {
    val lower = "abcdefghijklmnopqrstuvwxyz".toCharArray()
    val upper = lower.map( _.toUpper)
    lower ++ upper
  }
}
