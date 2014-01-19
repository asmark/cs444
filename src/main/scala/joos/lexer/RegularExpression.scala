package joos.lexer

abstract class RegularExpression {
  protected var entranceAtom: Atom
  protected var exitAtom: Atom

  def entrance = entranceAtom
  def entrance_= (_entranceAtom: Atom) = this.entranceAtom = _entranceAtom

  def exit = exitAtom
  def exit_= (exitAtom: Atom) = this.exitAtom = exitAtom
}

class Atom(var singleNode: NFANode) extends RegularExpression {
  protected var entranceAtom: Atom = this
  protected var exitAtom: Atom = this

  def node = singleNode
  def node_= (node: NFANode) = this.singleNode = node

  def addTransition(char: Char, dst: Atom) = {
    this.singleNode.addTransition(char, dst.singleNode)
  }
}

class Concatenation() extends RegularExpression {
  protected var entranceAtom: Atom = _
  protected var exitAtom: Atom = _

  def build(inputs: Array[RegularExpression]): RegularExpression = {
    assert(inputs.length > 1)
    this.entranceAtom = inputs(0).entrance
    this.exitAtom = inputs(inputs.length - 1).exit
    var idx = 0
    for (idx <- 0 to inputs.length - 2) {
      inputs(idx).exit.addTransition(NFANode.Epsilon, inputs(idx + 1).entrance)
    }
    this
  }
}

class Alternation() extends RegularExpression {
  protected var entranceAtom: Atom = _
  protected var exitAtom: Atom = _

  def build(inputs: Array[RegularExpression]): RegularExpression = {
    assert(inputs.length > 1)
    this.entranceAtom = new Atom(new NonAcceptingNFANode)
    this.exitAtom = new Atom(new NonAcceptingNFANode)
    var idx = 0
    for (idx <- 0 to inputs.length - 1) {
      this.entranceAtom.addTransition(NFANode.Epsilon, inputs(idx).entrance)
      inputs(idx).exit.addTransition(NFANode.Epsilon, this.exitAtom)
    }
    this
  }
}

class Closure() extends RegularExpression {
  protected var entranceAtom: Atom = _
  protected var exitAtom: Atom = _

  def build(atom: RegularExpression): RegularExpression = {
    // Loop the exit of the atom back to the entrance of the atom
    atom.exit.addTransition(NFANode.Epsilon, atom.entrance)
    // Connect the entrance and exit of the closure to the atom
    this.entranceAtom = new Atom(new NonAcceptingNFANode)
    this.exitAtom = new Atom(new NonAcceptingNFANode)
    this.entranceAtom.addTransition(NFANode.Epsilon, atom.entrance)
    this.entranceAtom.addTransition(NFANode.Epsilon, this.exitAtom)
    atom.exit.addTransition(NFANode.Epsilon, this.exitAtom)
    this
  }
}