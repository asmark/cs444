package joos.lexer

import org.scalatest.{FlatSpec, Matchers}
import joos.lexer.exceptions.ScanningException

class ScannerSpec extends FlatSpec with Matchers {
  private val CharacterA = 'A'
  private val CharacterB = 'B'
  private val CharacterC = 'C'

  private val TokenKind1 = "TOKEN-1"
  private val TokenKind2 = "TOKEN-2"

  private val testDfaNoLoops = NonAcceptingDFANode().
    addTransition(CharacterA, AcceptingDFANode(TokenKind1)).
    addTransition(CharacterB,
      AcceptingDFANode(TokenKind2).addTransition(CharacterA, NonAcceptingDFANode()))

  private val testDfaWithLoops: DFANode = NonAcceptingDFANode()
  testDfaWithLoops.addTransition(CharacterA, testDfaWithLoops).
    addTransition(CharacterB,
      AcceptingDFANode(TokenKind2).addTransition(CharacterA, AcceptingDFANode(TokenKind1)))

  private val testDfaDeadEnds = NonAcceptingDFANode().
    addTransition(CharacterA,
      NonAcceptingDFANode().addTransition(CharacterC,
        AcceptingDFANode(TokenKind1))).
    addTransition(CharacterB,
      NonAcceptingDFANode().addTransition(CharacterB,
        AcceptingDFANode(TokenKind2).addTransition(CharacterA,
          NonAcceptingDFANode().addTransition(CharacterC,
            NonAcceptingDFANode()))))


  "A state with no transition" should "backtrack once to accepting nodes" in {
    val scanner = new Scanner(testDfaDeadEnds)
    val input = Seq(CharacterB, CharacterB, CharacterA, CharacterC, CharacterB, CharacterB)

    input.foreach(char => scanner.parse(char))

    val tokens = scanner.getTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind2, TokenKind1, TokenKind2)
  }

  it should "backtrack twice to accepting nodes" in {
    val scanner = new Scanner(testDfaNoLoops)
    val input = Seq(CharacterB, CharacterA, CharacterB)

    input.foreach(char => scanner.parse(char))

    val tokens = scanner.getTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind2, TokenKind1, TokenKind2)
  }

  "A state with loops" should "loop correctly" in {
    val scanner = new Scanner(testDfaWithLoops)
    val input = Seq(CharacterA, CharacterA, CharacterA, CharacterB, CharacterA, CharacterA, CharacterB)

    input.foreach(char => scanner.parse(char))

    val tokens = scanner.getTokens()
    tokens.map(token => token.kind) should contain theSameElementsInOrderAs Seq(TokenKind1, TokenKind2)
  }

  "Non-tokenizable input" should "throw a scanning exception" in {
    val scanner = new Scanner(testDfaWithLoops)
    val input = Seq(CharacterA, CharacterA, CharacterA, CharacterB, CharacterB, CharacterB, CharacterA, CharacterA)

    intercept[ScanningException] {
      input.foreach(char => scanner.parse(char))
      scanner.getTokens()
    }
  }

}
