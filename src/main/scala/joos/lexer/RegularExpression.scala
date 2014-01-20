package joos.lexer

abstract class RegularExpression {
  protected var entranceNode: NFANode
  protected var exitNode: NFANode

  def entrance = entranceNode
  def entrance_= (entranceNode: NFANode) = this.entranceNode = entranceNode

  def exit = exitNode
  def exit_= (exitNode: NFANode) = this.exitNode = exitNode
}

class Atom(src: NFANode, dst: NFANode, input: Char) extends RegularExpression {
  protected var entranceNode: NFANode = src
  protected var exitNode: NFANode = dst
  protected var character = input

  entranceNode.addTransition(character, dst)

  def char = this.character
  def char_= (character: Char) = this.character = character
}

class Concatenation() extends RegularExpression {
  protected var entranceNode: NFANode = _
  protected var exitNode: NFANode = _

  def build(inputs: Array[RegularExpression]): RegularExpression = {
    assert(inputs.length > 1)
    this.entranceNode = inputs(0).entrance
    this.exitNode = inputs(inputs.length - 1).exit
    var idx = 0
    for (idx <- 0 to inputs.length - 2) {
      inputs(idx).exit.addTransition(NFANode.Epsilon, inputs(idx + 1).entrance)
    }
    this
  }
}

class Alternation() extends RegularExpression {
  protected var entranceNode: NFANode = _
  protected var exitNode: NFANode = _

  def build(inputs: Array[RegularExpression]): RegularExpression = {
    assert(inputs.length > 1)
    this.entranceNode = new NonAcceptingNFANode
    this.exitNode = new NonAcceptingNFANode
    var idx = 0
    for (idx <- 0 to inputs.length - 1) {
      this.entranceNode.addTransition(NFANode.Epsilon, inputs(idx).entrance)
      inputs(idx).exit.addTransition(NFANode.Epsilon, this.exitNode)
    }
    this
  }
}

class Closure() extends RegularExpression {
  protected var entranceNode: NFANode = _
  protected var exitNode: NFANode = _

  def build(atom: RegularExpression): RegularExpression = {
    // Loop the exit of the atom back to the entrance of the atom
    atom.exit.addTransition(NFANode.Epsilon, atom.entrance)
    // Connect the entrance and exit of the closure to the atom
    this.entranceNode = new NonAcceptingNFANode
    this.exitNode = new NonAcceptingNFANode
    this.entranceNode.addTransition(NFANode.Epsilon, atom.entrance)
    this.entranceNode.addTransition(NFANode.Epsilon, this.exitNode)
    atom.exit.addTransition(NFANode.Epsilon, this.exitNode)
    this
  }
}