package joos.lexer

import scala.collection.mutable

sealed abstract class NFANode {
  private val edges = mutable.HashMap[Char, List[NFANode]]()

  def addTransition(char: Char, node: NFANode): NFANode = {
    val neighbours = edges.getOrElse(char, List.empty[NFANode])
    edges.+=((char, neighbours :+ node))
    return this
  }

  def followTransition(char: Char): List[NFANode] = {
    return edges.getOrElse(char, List.empty[NFANode])
  }

  def isAccepting() : Option[Any] = this match {
    case NonAcceptingNFANode() => None
    case AcceptingNFANode(token) => Some(token)
}
  }

  case class NonAcceptingNFANode() extends NFANode
  case class AcceptingNFANode(token : Any) extends NFANode
