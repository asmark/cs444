package joos.lexer

object Token {
  final val FINAL = {
    val symbols = Array('f', 'i', 'n', 'a', 'l')
    val atoms = new Array[RegularExpression](symbols.length);
    var i = 0
    for (i <- 0 to symbols.length - 1) {
      if (i != symbols.length - 1) {
        atoms(i) = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), symbols(i))
      } else {
        atoms(i) = new Atom(NonAcceptingNFANode(), AcceptingNFANode("FINAL"), symbols(i))
      }
    }
    var concat = new Concatenation()
    concat.build(atoms)
  }
}
