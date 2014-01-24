package joos

import joos.exceptions.DuplicateTransitionException
import org.scalatest.{Matchers, FlatSpec}

class DFANodeSpec extends FlatSpec with Matchers {
  val CharacterA = 'A'
  val CharacterB = 'B'

  "A node with an 'A' transition" should "throw an exception when adding another" in {
    intercept[DuplicateTransitionException] {
      import joos.{AcceptingDfaNode, NonAcceptingDfaNode}
      NonAcceptingDfaNode().
        addTransition(CharacterA, NonAcceptingDfaNode()).
        addTransition(CharacterA, AcceptingDfaNode())
    }
  }

  "A node with an 'A' transition" should "follow that transition" in {
    import joos.{AcceptingDfaNode, NonAcceptingDfaNode, DfaNode}
    val node = NonAcceptingDfaNode().addTransition(CharacterA, AcceptingDfaNode("ID"))
    node.followTransition(CharacterA) match {
      case Some(neighbour: DfaNode) => neighbour.isAccepting() should be(Some("ID"))
      case None => fail("A transition should exist")
    }
  }

  "A node without an 'A' transition" should "not follow an A transition" in {
    import joos.{AcceptingDfaNode, NonAcceptingDfaNode}
    val node = NonAcceptingDfaNode().addTransition(CharacterB, AcceptingDfaNode("ID"))
    node.followTransition(CharacterA) should be(None)
  }

  "A node that is accepting" should "return the token" in {
    import joos.AcceptingDfaNode
    val token = "public"
    val node = AcceptingDfaNode(token)
    node.isAccepting() should be(Some(token))
  }

  "A node that is not accepting" should "return none" in {
    import joos.NonAcceptingDfaNode
    NonAcceptingDfaNode().isAccepting() should be(None)
  }


}
