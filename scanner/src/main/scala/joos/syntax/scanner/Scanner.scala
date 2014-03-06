package joos.syntax.scanner

import joos.syntax.automata._
import joos.syntax.tokens.TerminalToken
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
    generateTokens()
  }

  def scan(char: Char) {
    val nextNode = currentState().followTransition(char)
    nextNode match {
      case Some(node: DfaNode) => updatePath(char, node)
      case None => {
        reducePath()
        scan(char)
      }
    }
  }

  def generateTokens(): List[TerminalToken] = {
    while (!charPath.isEmpty) {
      reducePath()
    }
    tokens.toList
  }

  private def currentState(): DfaNode = {
    if (dfaPath.isEmpty) throw new ScanningException(s"Failed to scan at ${charPath}") else dfaPath.top
  }

  private def updatePath(char: Char, node: DfaNode) {
    dfaPath.push(node)
    charPath.push(char)
  }

  private def reducePath() {
    val extraChars = mutable.Stack[Char]()
    while (currentState().isAccepting.isEmpty) {
      if (charPath.isEmpty) throw new ScanningException(s"Failed to scan at ${extraChars}") else extraChars.push(charPath.pop())
      dfaPath.pop()
    }

    val tokenKind = currentState().isAccepting.get
    val lexeme = charPath.foldRight(mutable.StringBuilder.newBuilder)((char, builder) => builder.append(char))
    tokens += TerminalToken(lexeme.result(), tokenKind)

    dfaPath = mutable.Stack[DfaNode](root)
    charPath = mutable.Stack[Char]()
    extraChars.foreach(char => scan(char))
  }
}

object Scanner {
  def apply(dfa: Dfa): Scanner = {
    new Scanner(dfa)
  }
}
