package joos.lexer

import org.scalatest.FlatSpec

class NFANodeSpec extends FlatSpec {
  val CharacterA = 'A'
  val CharacterC = 'C'

  "A node with no neighbours" should "have no valid transitions" in {
    val node = NonAcceptingNFANode()
    assert(node.followTransition(CharacterC).isEmpty)
  }
  
  "A node with C neighbours" should "only have C transitiions" in {
    val node = NonAcceptingNFANode().addTransition(CharacterC, NonAcceptingNFANode())
    assert(node.followTransition(CharacterA).isEmpty)
    assert(node.followTransition(CharacterC).length == 1)
  }

  "A node with multiple C neighbours" should "return all neighbours" in {
    val node = NonAcceptingNFANode()
    val neighbour1 = NonAcceptingNFANode()
    val neighbour2 = NonAcceptingNFANode()
    node.addTransition(CharacterC, neighbour1)
    node.addTransition(CharacterC, neighbour2)
    neighbour1.addTransition(CharacterA, neighbour2)

    assert(node.followTransition(CharacterC).length == 2)
    assert(node.followTransition(CharacterC).contains(neighbour1))
    assert(node.followTransition(CharacterC).contains(neighbour2))

    assert(neighbour1.followTransition(CharacterA).length == 1)
    assert(neighbour1.followTransition(CharacterA).contains(neighbour2))

    assert(neighbour2.followTransition(CharacterA).length == 0)
  }

  "A node with a loop" should "have a transition to itself" in {
    val node = NonAcceptingNFANode()
    node.addTransition(CharacterA, node).addTransition(CharacterA, NonAcceptingNFANode())

    assert(node.followTransition(CharacterA).length == 2)
    assert(node.followTransition(CharacterA).contains(node))
  }

  "A non accepting node" should "be labelled as non accepting" in {
    val node = NonAcceptingNFANode()
    assert(node.isAccepting().isEmpty)
  }

  "An accepting node" should "be labelled as accepting" in {
    val token = "public"
    val node = AcceptingNFANode(token)
    assert(node.isAccepting().get == token)
  }
}
