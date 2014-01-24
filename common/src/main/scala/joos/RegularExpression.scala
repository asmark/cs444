package joos

abstract class RegularExpression {
  protected var entranceNode: NfaNode = _
  protected var exitNode: NfaNode = _

  def entrance = entranceNode

  def entrance_=(entranceNode: NfaNode) = this.entranceNode = entranceNode

  def exit = exitNode

  def exit_=(exitNode: NfaNode) = this.exitNode = exitNode

  def +(input: RegularExpression): RegularExpression = {
    this.exit.addTransition(NfaNode.Epsilon, input.entrance)
    this.exit = input.exit
    this
  }

  def * = {
    // Loop the exit of the atom back to the entrance of the atom
    val inner_entrance = this.entrance
    val inner_exit = this.exit
    inner_exit.addTransition(NfaNode.Epsilon, inner_entrance)
    // Connect the entrance and exit of the closure to the atom
    this.entranceNode = NonAcceptingNfaNode()
    this.exitNode = NonAcceptingNfaNode()
    this.entranceNode.addTransition(NfaNode.Epsilon, inner_entrance)
    this.entranceNode.addTransition(NfaNode.Epsilon, this.exitNode)
    inner_exit.addTransition(NfaNode.Epsilon, this.exitNode)
    this
  }

  def |(input: RegularExpression): RegularExpression = {
    val inner_entrance = this.entrance
    val inner_exit = this.exit

    this.entrance = NonAcceptingNfaNode()
    this.exit = NonAcceptingNfaNode()
    this.entrance.addTransition(NfaNode.Epsilon, inner_entrance)
    this.entrance.addTransition(NfaNode.Epsilon, input.entrance)
    inner_exit.addTransition(NfaNode.Epsilon, this.exit)
    input.exit.addTransition(NfaNode.Epsilon, this.exit)
    this
  }
}

case class Atom(src: NfaNode, dst: NfaNode, input: Char) extends RegularExpression {
  protected var character = input
  entranceNode = src
  exitNode = dst

  entranceNode.addTransition(character, exitNode)

  def char = this.character

  def char_=(character: Char) = this.character = character
}

object Atom {
  def apply(input: Char) = {
    new Atom(NonAcceptingNfaNode(), NonAcceptingNfaNode(), input)
  }
}

case class Concatenation(inputs: Seq[RegularExpression]) extends RegularExpression {
    if (inputs.length <= 1) {
      this.entrance = inputs(0).entrance
      this.exit = inputs(0).exit
    } else {
      this.entrance = inputs(0).entrance
      this.exit = inputs(inputs.length - 1).exit
      for (idx <- 0 to inputs.length - 2) {
        inputs(idx).exit.addTransition(NfaNode.Epsilon, inputs(idx + 1).entrance)
      }
    }
}

object Concatenation {
  def apply(str: String) = {
    new Concatenation(str.map(char => Atom(char)))
  }
}

case class Alternation(inputs: Seq[RegularExpression]) extends RegularExpression {
    if (inputs.length <= 1) {
      this.entrance = inputs(0).entrance
      this.exit = inputs(0).exit
    } else {
      this.entrance = NonAcceptingNfaNode()
      this.exit = NonAcceptingNfaNode()
      for (idx <- 0 to inputs.length - 1) {
        this.entrance.addTransition(NfaNode.Epsilon, inputs(idx).entrance)
        inputs(idx).exit.addTransition(NfaNode.Epsilon, this.exit)
      }
    }
}

object Alternation {
  def apply(str: String) = {
    new Alternation(str.map(char => Atom(NonAcceptingNfaNode(), NonAcceptingNfaNode(), char)))
  }
}

// Either accept the char or bypass it
case class Optional(input: RegularExpression) extends RegularExpression {
  // Loop the exit of the atom back to the entrance of the atom
  val inner_entrance = this.entrance
  val inner_exit = this.exit
  // Allow empty transition from start to end to bypass this
  this.entranceNode = NonAcceptingNfaNode()
  this.exitNode = NonAcceptingNfaNode()
  this.entranceNode.addTransition(NfaNode.Epsilon, inner_entrance)
  this.entranceNode.addTransition(NfaNode.Epsilon, this.exitNode)
  inner_exit.addTransition(NfaNode.Epsilon, this.exitNode)
}
