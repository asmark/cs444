package joos.lexer

import org.scalatest.{Matchers, FlatSpec}

class NFANodeSpec extends FlatSpec with Matchers {
  val CharacterA = 'A'
  val CharacterC = 'C'
  val Epsilon = NFANode.Epsilon

  "A node with no neighbours" should "have no valid transitions" in {
    val node = NonAcceptingNFANode()
    node.followTransition(CharacterC) should have length 0
  }

  "A node with C neighbours" should "only have C transitions" in {
    val node = NonAcceptingNFANode().addTransition(CharacterC, NonAcceptingNFANode())

    node.followTransition(CharacterA) should have length 0
    node.followTransition(CharacterC) should have length 1
  }

  "A node with multiple C neighbours" should "return all neighbours" in {
    val node = NonAcceptingNFANode()
    val neighbour1 = NonAcceptingNFANode()
    val neighbour2 = NonAcceptingNFANode()
    node.addTransition(CharacterC, neighbour1)
    node.addTransition(CharacterC, neighbour2)
    neighbour1.addTransition(CharacterA, neighbour2)

    node.followTransition(CharacterC) should have length 2
    node.followTransition(CharacterC) should contain(neighbour1)
    node.followTransition(CharacterC) should contain(neighbour2)

    neighbour1.followTransition(CharacterA) should have length 1
    neighbour1.followTransition(CharacterA) should contain(neighbour2)

    neighbour2.followTransition(CharacterA) should have length 0
  }

  "A node with a loop" should "have a transition to itself" in {
    val node = NonAcceptingNFANode()
    node.addTransition(CharacterA, node).addTransition(CharacterA, NonAcceptingNFANode())

    node.followTransition(CharacterA) should have length 2
    node.followTransition(CharacterA) should contain(node)
  }

  "A non accepting node" should "be labelled as non accepting" in {
    val node = NonAcceptingNFANode()

    node.isAccepting() should be(None)
  }

  "An accepting node" should "be labelled as accepting" in {
    val token = "public"
    val node = AcceptingNFANode(token)

    node.isAccepting() should be(Some(token))
  }

  "An epsilon closure involving loops" should "return all unique nodes that are part of the epsilon closure" in {
    val node = NonAcceptingNFANode()
    node.addTransition(Epsilon, NonAcceptingNFANode()).
      addTransition(Epsilon, AcceptingNFANode().addTransition(Epsilon, NonAcceptingNFANode())).
      addTransition(Epsilon, NonAcceptingNFANode().addTransition(Epsilon, node))

    node.getClosure(Epsilon) should have size 5
  }

  "An epsilon closure without loops" should "return all unique nodes that are part of the epsilon closure" in {
    val node = NonAcceptingNFANode()
    node.addTransition(Epsilon, NonAcceptingNFANode().addTransition(CharacterA, NonAcceptingNFANode())).
      addTransition(Epsilon, AcceptingNFANode().addTransition(Epsilon, NonAcceptingNFANode()))

    node.getClosure(Epsilon) should have size 4
  }
}
