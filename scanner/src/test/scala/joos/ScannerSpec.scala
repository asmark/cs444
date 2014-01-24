package joos

import joos.exceptions.ScanningException
import joos.regexp.{Concatenation, Alternation}
import joos.tokens.{TokenKind, TokenKindRegexp, Token}
import org.scalatest.{FlatSpec, Matchers}
import joos.scanner.Scanner

class ScannerSpec extends FlatSpec with Matchers {

  import joos.automata.{AcceptingDfaNode, NonAcceptingDfaNode, DfaNode}

  private val CharacterA = 'A'
  private val CharacterB = 'B'
  private val CharacterC = 'C'

  private val TokenKind1 = TokenKind.Id
  private val TokenKind2 = TokenKind.Dot

  private val testDfaNoLoops = NonAcceptingDfaNode().
    addTransition(CharacterA, AcceptingDfaNode(TokenKind1)).
    addTransition(CharacterB,
      AcceptingDfaNode(TokenKind2).addTransition(CharacterA, NonAcceptingDfaNode()))

  private val testDfaWithLoops: DfaNode = NonAcceptingDfaNode()
  testDfaWithLoops.addTransition(CharacterA, testDfaWithLoops).
    addTransition(CharacterB,
      AcceptingDfaNode(TokenKind2).addTransition(CharacterA, AcceptingDfaNode(TokenKind1)))

  private val testDfaDeadEnds = NonAcceptingDfaNode().
    addTransition(CharacterA,
      NonAcceptingDfaNode().addTransition(CharacterC,
        AcceptingDfaNode(TokenKind1))).
    addTransition(CharacterB,
      NonAcceptingDfaNode().addTransition(CharacterB,
        AcceptingDfaNode(TokenKind2).addTransition(CharacterA,
          NonAcceptingDfaNode().addTransition(CharacterC,
            NonAcceptingDfaNode()))))


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

  "An epsilon closure with multiple accepting states" should "accept the highest priority token" in {
    val testRegexp = Concatenation("final") := TokenKind.Id := TokenKind.Final

    val scanner = Scanner.forRegexp(testRegexp)

    "final".toCharArray.foreach(c => scanner.parse(c))
    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token(TokenKind.Final, "final"))
  }

  behavior of "A static word regular expression (final) to DFA conversion"

  it should "accept tokenizable (final) inputs" in {
    val scanner = Scanner.forRegexp(TokenKind.Final.getRegexp())

    "final".toCharArray.foreach(c => scanner.parse(c))
    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token(TokenKind.Final, "final"))
  }

  it should "reject non-tokenizable (final3) inputs" in {
    val scanner = Scanner.forRegexp(TokenKind.Final.getRegexp())

    intercept[ScanningException] {
      "final3".toCharArray.foreach(c => scanner.parse(c))
      scanner.getTokens()
    }
  }

  behavior of "An alternating word regular expression (T|test) to DFA conversion"

  it should "accept tokenizable (test) inputs" in {
    val testRegexp = Alternation("tT") + Concatenation("est") := TokenKind1

    val scanner = Scanner.forRegexp(testRegexp)

    "test".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token(TokenKind1, "test"))
  }

  it should "accept tokenizable (Test) inputs" in {
    val testRegexp = Alternation("tT") + Concatenation("est") := TokenKind1
    val scanner = Scanner.forRegexp(testRegexp)

    "Test".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token(TokenKind1, "Test"))
  }


  behavior of "A looping word regular expression (ID) to DFA conversion"

  it should "accept tokenizable (t998) inputs" in {
    val scanner = Scanner.forRegexp(TokenKind.Id.getRegexp())

    "t998".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token(TokenKind.Id, "t998"))
  }

  it should "reject non-tokenizable (9112abc) inputs" in {
    val scanner = Scanner.forRegexp(TokenKind.Id.getRegexp())

    intercept[ScanningException] {
      "9122abc".toCharArray.foreach(c => scanner.parse(c))
      scanner.getTokens()
    }
  }
}
