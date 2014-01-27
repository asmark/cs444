package joos.regexp

import joos.automata.{AcceptingNfaNode, NonAcceptingNfaNode, NfaNode}
import joos.tokens.TokenKind.TokenKind

abstract class RegularExpression {
  protected var entranceNode: NfaNode = _
  protected var exitNode: NfaNode = _

  def entrance = entranceNode

  def entrance_=(entranceNode: NfaNode) = this.entranceNode = entranceNode

  def exit = exitNode

  def exit_=(exitNode: NfaNode) = this.exitNode = exitNode

  // Concatenation
  def +(input: RegularExpression): RegularExpression = {
    Concatenation(Seq(this, input))
  }

  // Multiple (0 or more instances)
  def * = {
    val inner_entrance = this.entrance
    val inner_exit = this.exit
    inner_exit.addTransition(NfaNode.Epsilon, inner_entrance)
    this.entranceNode = NonAcceptingNfaNode()
    this.exitNode = NonAcceptingNfaNode()
    this.entranceNode.addTransition(NfaNode.Epsilon, inner_entrance)
    this.entranceNode.addTransition(NfaNode.Epsilon, this.exitNode)
    inner_exit.addTransition(NfaNode.Epsilon, this.exitNode)
    this
  }

  // Alternation
  def |(input: RegularExpression): RegularExpression = {
    Alternation(Seq(this, input))
  }

  // Optional (O or 1 instances)
  def unary_~ : RegularExpression = {
    val inner_entrance = this.entrance
    val inner_exit = this.exit
    this.entranceNode = NonAcceptingNfaNode()
    this.exitNode = NonAcceptingNfaNode()
    this.entranceNode.addTransition(NfaNode.Epsilon, inner_entrance)
    this.entranceNode.addTransition(NfaNode.Epsilon, this.exitNode)
    inner_exit.addTransition(NfaNode.Epsilon, this.exitNode)
    this
  }

  // Mark as accepting
  def :=(kind: TokenKind): RegularExpression = {
    this + Atom(NonAcceptingNfaNode(), AcceptingNfaNode(kind), NfaNode.Epsilon)
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
    new Alternation(str.map(char => Atom(char)))
  }
}
