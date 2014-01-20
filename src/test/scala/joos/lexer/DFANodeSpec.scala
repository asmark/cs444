package joos.lexer

import org.scalatest.{Matchers, FlatSpec}

class DFANodeSpec extends FlatSpec with Matchers {
  val CharacterA = 'A'
  val CharacterB = 'B'

  "A node with an 'A' transition" should "throw an exception when adding another" in {
    intercept[DuplicateTransitionException] {
      NonAcceptingDFANode().
        addTransition(CharacterA, NonAcceptingDFANode()).
        addTransition(CharacterA, AcceptingDFANode())
    }
  }

  "A node with an 'A' transition" should "follow that transition" in {
    val node = NonAcceptingDFANode().addTransition(CharacterA, AcceptingDFANode("ID"))
    node.followTransition(CharacterA) match {
      case Some(neighbour: DFANode) => neighbour.isAccepting() should be (Some("ID"))
      case None => fail("A transition should exist")
    }
  }

  "A node without an 'A' transition" should "not follow an A transition" in {
    val node = NonAcceptingDFANode().addTransition(CharacterB, AcceptingDFANode("ID"))
    node.followTransition(CharacterA) should be (None)
  }

  "A node that is accepting" should "return the token" in {
    val token = "public"
    val node = AcceptingDFANode(token)
    node.isAccepting() should be (Some(token))
  }

  "A node that is not accepting" should "return none" in  {
    NonAcceptingDFANode().isAccepting() should be (None)
  }


}
