package joos.lexer

class RegularExpression {
  protected var entranceNode: NFANode = _
  protected var exitNode: NFANode = _

  def entrance = entranceNode
  def entrance_= (entranceNode: NFANode) = this.entranceNode = entranceNode

  def exit = exitNode
  def exit_= (exitNode: NFANode) = this.exitNode = exitNode

  def +(input: RegularExpression): RegularExpression = {
    this.exit.addTransition(NFANode.Epsilon, input.entrance)
    this.exit = input.exit
    this
  }

  def * = {
    // Loop the exit of the atom back to the entrance of the atom
    val inner_entrance = this.entrance
    val inner_exit = this.exit
    inner_exit.addTransition(NFANode.Epsilon, inner_entrance)
    // Connect the entrance and exit of the closure to the atom
    this.entranceNode = new NonAcceptingNFANode
    this.exitNode = new NonAcceptingNFANode
    this.entranceNode.addTransition(NFANode.Epsilon, inner_entrance)
    this.entranceNode.addTransition(NFANode.Epsilon, this.exitNode)
    inner_exit.addTransition(NFANode.Epsilon, this.exitNode)
    this
  }

  def |(input:RegularExpression) : RegularExpression = {
    val inner_entrace = this.entrance
    val inner_exit = this.exit

    this.entrance = new NonAcceptingNFANode
    this.exit = new NonAcceptingNFANode
    this.entrance.addTransition(NFANode.Epsilon, inner_entrace)
    this.entrance.addTransition(NFANode.Epsilon, input.entrance)
    inner_exit.addTransition(NFANode.Epsilon, this.exit)
    input.exit.addTransition(NFANode.Epsilon, this.exit)
    this
  }
}

object RegularExpression {
  def concatAll(inputs: Array[RegularExpression]): RegularExpression = {
    if (inputs.length > 1)
       return inputs(0)
    val regexp = new RegularExpression()
    regexp.entrance = inputs(0).entrance
    regexp.exit = inputs(inputs.length - 1).exit
    for (idx <- 0 to inputs.length - 2) {
      inputs(idx).exit.addTransition(NFANode.Epsilon, inputs(idx + 1).entrance)
    }
    regexp
  }

  def alterAll(inputs: Array[RegularExpression]): RegularExpression = {
    if (inputs.length > 1)
      return inputs(0)
    val regexp = new RegularExpression()
    regexp.entrance = new NonAcceptingNFANode
    regexp.exit = new NonAcceptingNFANode
    for (idx <- 0 to inputs.length - 1) {
      regexp.entrance.addTransition(NFANode.Epsilon, inputs(idx).entrance)
      inputs(idx).exit.addTransition(NFANode.Epsilon, regexp.exit)
    }
    regexp
  }
}

class Atom(src: NFANode, dst: NFANode, input: Char) extends RegularExpression {
  entranceNode = src
  exitNode = dst
  protected var character = input

  entranceNode.addTransition(character, dst)

  def char = this.character
  def char_= (character: Char) = this.character = character
}