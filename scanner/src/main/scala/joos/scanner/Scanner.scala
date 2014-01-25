package joos.scanner

import joos.automata._
import joos.exceptions.ScanningException
import joos.tokens.Token
import scala.Some
import scala.collection.mutable
import scala.io.Source

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
  def apply(dfa: DfaNode): Scanner = {
    return new Scanner(dfa)
  }
}
