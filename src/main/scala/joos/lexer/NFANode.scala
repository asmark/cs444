package joos.lexer

import scala.collection.mutable

sealed abstract class NFANode {
  val edges = mutable.HashMap[Char, List[NFANode]]()

  def addTransition(char: Char, node: NFANode): NFANode = {
    val neighbours = edges.getOrElse(char, List.empty[NFANode])
    edges.+=((char, neighbours :+ node))
    return this
  }

  def followTransition(char: Char): List[NFANode] = {
    return edges.getOrElse(char, List.empty[NFANode])
  }

  def isAccepting(): Option[Any] = this match {
    case NonAcceptingNFANode() => None
    case AcceptingNFANode(token) => Some(token)
  }

  def getClosure(char: Char): mutable.Set[NFANode] = {
    val closure = mutable.HashSet.empty[NFANode]
    val nodesToExamine = mutable.Queue(this)

    while (!nodesToExamine.isEmpty) {
      val node = nodesToExamine.dequeue()
      closure.+=(node)
      node.followTransition(char).
        withFilter(neighbour => !closure.contains(neighbour)).
        foreach(neighbour => nodesToExamine.enqueue(neighbour))
    }
    return closure
  }

  override def equals(other: Any): Boolean = super.equals(other)
}

object NFANode {
  final val Epsilon = 'ε'
}

case class NonAcceptingNFANode() extends NFANode

case class AcceptingNFANode(token: Any) extends NFANode
