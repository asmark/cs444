package joos.lexer

package object TokenKind {
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
    new MultiConcat(atoms)
  }
}
