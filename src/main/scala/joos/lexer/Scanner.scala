package joos.lexer

import scala.collection.mutable
import joos.lexer.exceptions.ScanningException
import scala.io.Source

class Scanner(root: DFANode) {
  private var dfaPath = mutable.Stack[DFANode](root)
  private var charPath = mutable.Stack[Char]()
  private val tokens = mutable.MutableList[Token]()

  def tokenize(file: String): List[Token] = {
    Source.fromFile(file).foreach(char => parse(char))
    return getTokens()
  }

  def parse(char: Char) {
    val nextNode = getCurrentNode().followTransition(char)
    nextNode match {
      case Some(node: DFANode) => updatePath(char, node)
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

  private def getCurrentNode(): DFANode = {
    return if (dfaPath.isEmpty) throw new ScanningException() else dfaPath.top
  }

  private def updatePath(char: Char, node: DFANode) {
    dfaPath.push(node)
    charPath.push(char)
  }

  private def reducePath() {
    val extraChars = mutable.Stack[Char]()
    while (getCurrentNode().isAccepting().isEmpty) {
      val lastChar = if (charPath.isEmpty) throw new ScanningException() else charPath.top
      extraChars.push(lastChar)
      dfaPath.pop()
    }

    val tokenKind = dfaPath.top.isAccepting().get
    val lexeme = charPath.foldRight(mutable.StringBuilder.newBuilder)((char, builder) => builder.append(char))
    tokens += new Token(tokenKind, lexeme.result())

    dfaPath = mutable.Stack[DFANode](root)
    charPath = mutable.Stack[Char]()
    extraChars.foreach(char => parse(char))
  }
}

object Scanner {
  def fromRegexp(regexp : Any): Scanner = {
    // TODO: Before we can do this we need to define priorities and our TokenKind class
    return null
  }
}
