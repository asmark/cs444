package joos.automata

import joos.automata.exceptions.DuplicateTransitionException
import joos.regexp.RegularExpression
import joos.tokens.TokenKind
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

object DfaNode {
  private def getEpsilonClosure(nfaNodes: Set[NfaNode]): Set[NfaNode] = {
    val epsilonClosure = mutable.Set[NfaNode]()
    nfaNodes.foreach(node => epsilonClosure ++= node.getClosure(NfaNode.Epsilon))
    return epsilonClosure.toSet
  }

  private def newDfaNode(nfaNodes: Set[NfaNode]): DfaNode = {

    val acceptingKinds = nfaNodes.collect {
      case node: AcceptingNfaNode => node.kind
    }

    return if (acceptingKinds.isEmpty) NonAcceptingDfaNode() else AcceptingDfaNode(TokenKind.getHighestPriority(acceptingKinds))
  }

  private def getOrCreateDfaNode(dfaNodes: mutable.HashMap[Set[NfaNode], DfaNode], nfaNodes: Set[NfaNode]): DfaNode = {
    return dfaNodes.get(nfaNodes) match {
      case Some(dfaNode: DfaNode) => return dfaNode
      case None => {
        val dfaNode = newDfaNode(nfaNodes)
        dfaNodes += ((nfaNodes, dfaNode))
        return dfaNode
      }
    }
  }

  private def unionTransitions(nfaNodes: Set[NfaNode]): mutable.HashMap[Char, Set[NfaNode]] = {
    val unionTransitions = mutable.HashMap[Char, Set[NfaNode]]()

    nfaNodes.foreach {
      node =>
        node.edges.foreach {
          transition =>
            val char = transition._1
            val neighbours = transition._2

            val existingNeighbours = unionTransitions.getOrElse(char, Set.empty[NfaNode])

            unionTransitions += ((char, existingNeighbours ++ neighbours))
        }
    }

    return unionTransitions
  }

  def apply(regexp: RegularExpression): DfaNode = {
    val dfaNodeSet = mutable.HashMap.empty[Set[NfaNode], DfaNode]
    val rootNode = getEpsilonClosure(Set(regexp.entrance))

    val visitClosures = mutable.Queue(rootNode)
    val visitedClosures = mutable.HashSet(rootNode)

    while (!visitClosures.isEmpty) {
      val closure = visitClosures.dequeue()

      val dfaNode = getOrCreateDfaNode(dfaNodeSet, closure)
      val transitions = unionTransitions(closure)

      transitions.withFilter(transition => transition._1 != NfaNode.Epsilon).foreach {
        transition =>
          val char = transition._1
          val neighbourClosure = getEpsilonClosure(transition._2)

          dfaNode.edges += ((char, getOrCreateDfaNode(dfaNodeSet, neighbourClosure)))

          if (!visitedClosures.contains(neighbourClosure)) {
            visitClosures.enqueue(neighbourClosure)
            visitedClosures += neighbourClosure
          }
      }
    }

    return dfaNodeSet.get(rootNode).get
  }
}
