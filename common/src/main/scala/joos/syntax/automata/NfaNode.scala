package joos.syntax.automata

import joos.syntax.tokens.TokenKind.TokenKind
import scala.collection.mutable

sealed abstract class NfaNode {
  val edges = mutable.HashMap[Char, Set[NfaNode]]()

  def addTransition(char: Char, node: NfaNode): NfaNode = {
    val neighbours = edges.getOrElse(char, Set.empty[NfaNode])
    edges += ((char, neighbours + node))
    this
  }

  def followTransition(char: Char): Set[NfaNode] = {
    edges.getOrElse(char, Set.empty[NfaNode])
  }

  def isAccepting: Option[TokenKind] = this match {
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
    closure.toSet
  }

  override def equals(other: Any): Boolean = super.equals(other)
}

object NfaNode {
  final val Epsilon = 'ε'
}

case class NonAcceptingNfaNode() extends NfaNode

case class AcceptingNfaNode(kind: TokenKind) extends NfaNode
