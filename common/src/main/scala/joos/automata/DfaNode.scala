package joos.automata

import joos.automata.exceptions.DuplicateTransitionException
import joos.tokens.TokenKind.TokenKind
import scala.collection.mutable

sealed abstract class DfaNode {
  val edges = mutable.HashMap[Char, DfaNode]()

  def addTransition(char: Char, node: DfaNode): DfaNode = {
    edges.put(char, node) match {
      case Some(node: DfaNode) => throw new DuplicateTransitionException("Transition to %c already exists".format(char))
      case None => return this
    }
  }

  def followTransition(char: Char): Option[DfaNode] = {
    return edges.get(char)
  }

  def isAccepting(): Option[TokenKind] = this match {
    case NonAcceptingDfaNode() => None
    case AcceptingDfaNode(token) => Some(token)
  }

  override def equals(other: Any): Boolean = super.equals(other)
}

case class NonAcceptingDfaNode() extends DfaNode

case class AcceptingDfaNode(kind: TokenKind) extends DfaNode
