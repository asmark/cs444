package joos.lexer

import scala.collection.mutable
import joos.lexer.exceptions.DuplicateTransitionException

sealed abstract class DFANode {
  val edges = mutable.HashMap[Char, DFANode]()

  def addTransition(char: Char, node: DFANode): DFANode = {
    edges.put(char, node) match {
      case Some(node: DFANode) => throw new DuplicateTransitionException("Transition to %c already exists".format(char))
      case None => return this
    }
  }

  def followTransition(char: Char): Option[DFANode] = {
    return edges.get(char)
  }

  def isAccepting(): Option[Any] = this match {
    case NonAcceptingDFANode() => None
    case AcceptingDFANode(token) => Some(token)
  }

  override def equals(other: Any): Boolean = super.equals(other)
}

case class NonAcceptingDFANode() extends DFANode

case class AcceptingDFANode(token: Any) extends DFANode
