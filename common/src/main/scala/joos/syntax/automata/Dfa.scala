package joos.syntax.automata

import java.io._
import java.util.regex.Pattern
import joos.syntax.regexp.RegularExpression
import joos.syntax.tokens.TokenKind
import joos.syntax.tokens.TokenKind.TokenKind
import scala.Some
import scala.Tuple2
import scala.collection.mutable


class Dfa(val root: DfaNode) {

  def serialize(ostream: OutputStream) {
    val writer = new PrintWriter(new BufferedWriter(new OutputStreamWriter(ostream)))
    var nextId = 0
    val nodeToId = mutable.HashMap[DfaNode, Integer]()

    val dfsStack = mutable.Stack[DfaNode](root)
    while (!dfsStack.isEmpty) {
      val node = dfsStack.pop()

      if (!nodeToId.contains(node)) {
        nodeToId.put(node, nextId)
        nextId += 1
      }

      node.edges.values.foreach {
        neighbour =>
          if (!nodeToId.contains(neighbour) && !dfsStack.contains(neighbour)) {
            dfsStack.push(neighbour)
          }
      }
    }

    writer.append(s"${nextId}\n")

    type Row = mutable.MutableList[(Integer, Integer)]
    type AdjacencyList = Array[Row]

    // Construct the adjacency list and list all nodes followed by their accepting tokens (if any)
    val adjacencyList = new AdjacencyList(nextId)
    nodeToId.foreach {
      entry =>
        val node = entry._1
        val id = entry._2
        adjacencyList(id) = new Row()

        node.edges.foreach {
          transition =>
            adjacencyList(id) += ((nodeToId(transition._2), transition._1.toInt))
        }

        node match {
          case AcceptingDfaNode(token: TokenKind) => writer.append(s"${id}:${token}\n")
          case NonAcceptingDfaNode() => writer.append(s"${id}:\n")
        }
        writer.flush()
    }

    // For each node, list its edges
    adjacencyList.indices.foreach {
      i =>
        writer.append(s"${i}:${adjacencyList(i).mkString(",")}\n")
        writer.flush()
    }
  }

}

object Dfa {

  private val EdgeMatcher = Pattern.compile("\\(([0-9]+),([0-9]+)\\)+")

  private def getEpsilonClosure(nfaNodes: Set[NfaNode]): Set[NfaNode] = {
    val epsilonClosure = mutable.Set[NfaNode]()
    nfaNodes.foreach(node => epsilonClosure ++= node.getClosure(NfaNode.Epsilon))
    epsilonClosure.toSet
  }

  private def newDfaNode(nfaNodes: Set[NfaNode]): DfaNode = {

    val acceptingKinds = nfaNodes.collect {
      case node: AcceptingNfaNode => node.kind
    }

    if (acceptingKinds.isEmpty) NonAcceptingDfaNode()
    else AcceptingDfaNode(
      TokenKind
        .getHighestPriority(acceptingKinds)
    )
  }

  private def getOrCreateDfaNode(dfaNodes: mutable.HashMap[Set[NfaNode], DfaNode], nfaNodes: Set[NfaNode]): DfaNode = {
    dfaNodes.get(nfaNodes) match {
      case Some(dfaNode: DfaNode) => dfaNode
      case None => {
        val dfaNode = newDfaNode(nfaNodes)
        dfaNodes += ((nfaNodes, dfaNode))
        dfaNode
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

    unionTransitions
  }

  def apply(regexp: RegularExpression): Dfa = {
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

          if (!visitedClosures.contains(neighbourClosure) && !visitClosures.contains(neighbourClosure)) {
            visitClosures.enqueue(neighbourClosure)
            visitedClosures += neighbourClosure
          }
      }
    }

    new Dfa(dfaNodeSet(rootNode))
  }

  def apply(root: DfaNode): Dfa = {
    new Dfa(root)
  }

  def deserialize(istream: InputStream): Dfa = {
    val reader = new BufferedReader(new InputStreamReader(istream))
    val numNodes = reader.readLine().toInt

    val nodeMap = mutable.HashMap.empty[Integer, DfaNode]

    for (i <- 0 until numNodes) {
      val nodeData = reader.readLine().split(":")
      nodeData.length match {
        case 2 => nodeMap += ((nodeData(0).toInt, AcceptingDfaNode(TokenKind.withName(nodeData(1)))))
        case 1 => nodeMap += ((nodeData(0).toInt, NonAcceptingDfaNode()))
      }
    }

    while (reader.ready()) {
      val nodeData = reader.readLine().split(":")
      if (nodeData.length > 1) {
        val nodeNum = nodeData(0).toInt
        val edges = nodeData(1)

        val matcher = EdgeMatcher.matcher(edges)
        while (matcher.find()) {
          val neighbour = matcher.group(1).toInt
          val char = matcher.group(2).toInt.toChar
          nodeMap(nodeNum).addTransition(char, nodeMap(neighbour))
        }
      }
    }

    Dfa(nodeMap(0))
  }

}
