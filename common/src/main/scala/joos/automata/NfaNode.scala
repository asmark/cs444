package joos.automata

import joos.tokens.TokenKind.TokenKind
import scala.collection.mutable

sealed abstract class NfaNode {
  val edges = mutable.HashMap[Char, Set[NfaNode]]()

  def addTransition(char: Char, node: NfaNode): NfaNode = {
    val neighbours = edges.getOrElse(char, Set.empty[NfaNode])
    edges += ((char, neighbours + node))
    return this
  }

  def followTransition(char: Char): Set[NfaNode] = {
    return edges.getOrElse(char, Set.empty[NfaNode])
  }

  def isAccepting(): Option[TokenKind] = this match {
    case NonAcceptingNfaNode() => None
    case AcceptingNfaNode(token) => Some(token)
  }

  def getClosure(char: Char): Set[NfaNode] = {
    val closure = mutable.HashSet.empty[NfaNode]
    val nodesToExamine = mutable.Queue(this)

    while (!nodesToExamine.isEmpty) {
      val node = nodesToExamine.dequeue()
      closure += node
      node.followTransition(char).
        withFilter(neighbour => !closure.contains(neighbour)).
        foreach(neighbour => nodesToExamine.enqueue(neighbour))
    }
    return closure.toSet
  }

  override def equals(other: Any): Boolean = super.equals(other)
}

object NfaNode {
  final val Epsilon = 'ε'
}

case class NonAcceptingNfaNode() extends NfaNode

case class AcceptingNfaNode(kind: TokenKind) extends NfaNode
