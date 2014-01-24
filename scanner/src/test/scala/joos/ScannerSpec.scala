package joos

import joos.automata.{AcceptingNfaNode, NfaNode, NonAcceptingNfaNode}
import joos.exceptions.ScanningException
import joos.regexp.Atom
import joos.tokens.TokenKindRegexp
import org.scalatest.{FlatSpec, Matchers}

class ScannerSpec extends FlatSpec with Matchers {

  import joos.automata.{AcceptingDfaNode, NonAcceptingDfaNode, DfaNode}

  private val CharacterA = 'A'
  private val CharacterB = 'B'
  private val CharacterC = 'C'

  private val TokenKind1 = "TOKEN-1"
  private val TokenKind2 = "TOKEN-2"

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

  behavior of "A static word regular expression (final) to DFA conversion"

  it should "accept tokenizable (final) inputs" in {
    val scanner = Scanner.forRegexp(TokenKindRegexp.Final)

    "final".toCharArray.foreach(c => scanner.parse(c))
    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token("final", "final"))
  }

  it should "reject non-tokenizable (final3) inputs" in {
    val scanner = Scanner.forRegexp(TokenKindRegexp.Final)

    intercept[ScanningException] {
      "final3".toCharArray.foreach(c => scanner.parse(c))
      scanner.getTokens()
    }
  }

  behavior of "An alternating word regular expression (T|test) to DFA conversion"

  it should "accept tokenizable (test) inputs" in {
    val TestRegexp = (Atom('T') | Atom('t')) + Atom('e') + Atom('s') + Atom('t') + Atom(NonAcceptingNfaNode(), AcceptingNfaNode("test"), NfaNode.Epsilon)

    val scanner = Scanner.forRegexp(TestRegexp)

    "test".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token("test", "test"))
  }

  it should "accept tokenizable (Test) inputs" in {
    val TestRegexp = (Atom('T') | Atom('t')) + Atom('e') + Atom('s') + Atom('t') + Atom(NonAcceptingNfaNode(), AcceptingNfaNode("test"), NfaNode.Epsilon)
    val scanner = Scanner.forRegexp(TestRegexp)

    "Test".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token("test", "Test"))
  }


  behavior of "A looping word regular expression (ID) to DFA conversion"

  it should "accept tokenizable (t998) inputs" in {
    val scanner = Scanner.forRegexp(TokenKindRegexp.ID)

    "t998".toCharArray.foreach(c => scanner.parse(c))

    val tokens = scanner.getTokens()
    tokens should have length 1
    tokens should contain(new Token("ID", "t998"))
  }

  it should "reject non-tokenizable (9112abc) inputs" in {
    val scanner = Scanner.forRegexp(TokenKindRegexp.ID)

    intercept[ScanningException] {
      "9122abc".toCharArray.foreach(c => scanner.parse(c))
      scanner.getTokens()
    }
  }
}
