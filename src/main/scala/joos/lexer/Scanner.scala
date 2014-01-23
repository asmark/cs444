package joos.lexer

import scala.collection.mutable
import joos.lexer.exceptions.ScanningException
import scala.io.Source
import joos.lexer.NfaNode.Epsilon

class Scanner(root: DfaNode) {
  private var dfaPath = mutable.Stack[DfaNode](root)
  private var charPath = mutable.Stack[Char]()
  private val tokens = mutable.MutableList[Token]()

  def tokenize(file: String): List[Token] = {
    Source.fromFile(file).foreach(char => parse(char))
    return getTokens()
  }

  def parse(char: Char) {
    val nextNode = getCurrentNode().followTransition(char)
    nextNode match {
      case Some(node: DfaNode) => updatePath(char, node)
      case None => {
        reducePath()
        parse(char)
      }
    }
  }

  def getTokens(): List[Token] = {
    while (!charPath.isEmpty) {
      reducePath()
    }
    return tokens.toList
  }

  private def getCurrentNode(): DfaNode = {
    return if (dfaPath.isEmpty) throw new ScanningException() else dfaPath.top
  }

  private def updatePath(char: Char, node: DfaNode) {
    dfaPath.push(node)
    charPath.push(char)
  }

  private def reducePath() {
    val extraChars = mutable.Stack[Char]()
    while (getCurrentNode().isAccepting().isEmpty) {
      if (charPath.isEmpty) throw new ScanningException() else extraChars.push(charPath.pop())
      dfaPath.pop()
    }

    val tokenKind = dfaPath.top.isAccepting().get
    val lexeme = charPath.foldRight(mutable.StringBuilder.newBuilder)((char, builder) => builder.append(char))
    tokens += new Token(tokenKind, lexeme.result())

    dfaPath = mutable.Stack[DfaNode](root)
    charPath = mutable.Stack[Char]()
    extraChars.foreach(char => parse(char))
  }
}

object Scanner {

  private def getEpsilonClosure(nfaNodes: Set[NfaNode]): Set[NfaNode] = {
    val epsilonClosure = mutable.Set[NfaNode]()
    nfaNodes.foreach(node => epsilonClosure ++= node.getClosure(Epsilon))
    return epsilonClosure.toSet
  }

  private def newDfaNode(nfaNodes: Set[NfaNode]): DfaNode = {
    val acceptingToken = nfaNodes.collectFirst {
      case node: AcceptingNfaNode => node.tokenKind
    }
    return acceptingToken match {
      case Some(tokenKind: Any) => AcceptingDfaNode(tokenKind)
      case None => NonAcceptingDfaNode()
    }
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

  def forRegexp(regexp: RegularExpression): Scanner = {
    val dfaNodeSet = mutable.HashMap.empty[Set[NfaNode], DfaNode]
    val rootNode = getEpsilonClosure(Set.apply(regexp.entrance))

    val visitClosures = mutable.Queue.apply(rootNode)
    val visitedClosures = mutable.HashSet.apply(rootNode)

    while (!visitClosures.isEmpty) {
      val closure = visitClosures.dequeue()

      val dfaNode = getOrCreateDfaNode(dfaNodeSet, closure)
      val transitions = unionTransitions(closure)

      transitions.withFilter(transition => transition._1 != Epsilon).foreach {
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

    return new Scanner(dfaNodeSet.get(rootNode).get)
  }
}
