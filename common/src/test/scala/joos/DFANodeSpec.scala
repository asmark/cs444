package joos

import joos.automata.exceptions.DuplicateTransitionException
import joos.automata.{AcceptingDfaNode, NonAcceptingDfaNode}
import joos.tokens.TokenKind
import org.scalatest.{Matchers, FlatSpec}

class DFANodeSpec extends FlatSpec with Matchers {
  val CharacterA = 'A'
  val CharacterB = 'B'

  behavior of "A node with an 'A' transition"

  it should "throw an exception when adding another" in {
    intercept[DuplicateTransitionException] {
      NonAcceptingDfaNode().
        addTransition(CharacterA, NonAcceptingDfaNode()).
        addTransition(CharacterA, AcceptingDfaNode(TokenKind.Id))
    }
  }

  it should "follow that transition" in {
    import joos.automata.{AcceptingDfaNode, NonAcceptingDfaNode, DfaNode}
    val node = NonAcceptingDfaNode().addTransition(CharacterA, AcceptingDfaNode(TokenKind.Id))
    node.followTransition(CharacterA) match {
      case Some(neighbour: DfaNode) => neighbour.isAccepting() should be(Some(TokenKind.Id))
      case None => fail("A transition should exist")
    }
  }

  "A node without an 'A' transition" should "not follow an A transition" in {
    val node = NonAcceptingDfaNode().addTransition(CharacterB, AcceptingDfaNode(TokenKind.Final))
    node.followTransition(CharacterA) should be(None)
  }

  "A node that is accepting" should "return the token" in {
    val token = TokenKind.Final
    val node = AcceptingDfaNode(token)
    node.isAccepting() should be(Some(token))
  }

  "A node that is not accepting" should "return none" in {
    NonAcceptingDfaNode().isAccepting() should be(None)
  }

}
