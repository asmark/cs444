package joos.scanner

import joos.automata._
import joos.tokens.TerminalToken
import scala.Some
import scala.collection.mutable
import scala.io.Source

class Scanner(dfa: Dfa) {

  val root = dfa.root

  private var dfaPath = mutable.Stack[DfaNode](root)
  private var charPath = mutable.Stack[Char]()
  private val tokens = mutable.MutableList[TerminalToken]()

  def tokenize(file: Source): List[TerminalToken] = {
    file foreach scan
    getTokens()
  }

  def scan(char: Char) {
    val nextNode = getCurrentNode().followTransition(char)
    nextNode match {
      case Some(node: DfaNode) => updatePath(char, node)
      case None => {
        reducePath()
        scan(char)
      }
    }
  }

  def getTokens(): List[TerminalToken] = {
    while (!charPath.isEmpty) {
      reducePath()
    }
    return tokens.toList
  }

  private def getCurrentNode(): DfaNode = {
    return if (dfaPath.isEmpty) throw new ScanningException(s"Failed to scan at ${charPath}") else dfaPath.top
  }

  private def updatePath(char: Char, node: DfaNode) {
    dfaPath.push(node)
    charPath.push(char)
  }

  private def reducePath() {
    val extraChars = mutable.Stack[Char]()
    while (getCurrentNode().isAccepting().isEmpty) {
      if (charPath.isEmpty) throw new ScanningException(s"Failed to scan at ${extraChars}") else extraChars.push(charPath.pop())
      dfaPath.pop()
    }

    val tokenKind = dfaPath.top.isAccepting().get
    val lexeme = charPath.foldRight(mutable.StringBuilder.newBuilder)((char, builder) => builder.append(char))
    tokens += TerminalToken(lexeme.result(), tokenKind)

    dfaPath = mutable.Stack[DfaNode](root)
    charPath = mutable.Stack[Char]()
    extraChars.foreach(char => scan(char))
  }
}

object Scanner {
  def apply(dfa: Dfa): Scanner = {
    return new Scanner(dfa)
  }
}
