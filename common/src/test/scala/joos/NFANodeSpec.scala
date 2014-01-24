package joos

import org.scalatest.{Matchers, FlatSpec}

class NFANodeSpec extends FlatSpec with Matchers {

  import joos.automata.NfaNode

  val CharacterA = 'A'
  val CharacterC = 'C'
  val Epsilon = NfaNode.Epsilon

  "A node with no neighbours" should "have no valid transitions" in {
    import joos.automata.NonAcceptingNfaNode
    val node = NonAcceptingNfaNode()
    node.followTransition(CharacterC) should have size 0
  }

  "A node with C neighbours" should "only have C transitions" in {
    import joos.automata.NonAcceptingNfaNode
    val node = NonAcceptingNfaNode().addTransition(CharacterC, NonAcceptingNfaNode())

    node.followTransition(CharacterA) should have size 0
    node.followTransition(CharacterC) should have size 1
  }

  "A node with multiple C neighbours" should "return all neighbours" in {
    import joos.automata.NonAcceptingNfaNode
    val node = NonAcceptingNfaNode()
    val neighbour1 = NonAcceptingNfaNode()
    val neighbour2 = NonAcceptingNfaNode()
    node.addTransition(CharacterC, neighbour1)
    node.addTransition(CharacterC, neighbour2)
    neighbour1.addTransition(CharacterA, neighbour2)

    node.followTransition(CharacterC) should have size 2
    node.followTransition(CharacterC) should contain(neighbour1)
    node.followTransition(CharacterC) should contain(neighbour2)

    neighbour1.followTransition(CharacterA) should have size 1
    neighbour1.followTransition(CharacterA) should contain(neighbour2)

    neighbour2.followTransition(CharacterA) should have size 0
  }

  "A node with a loop" should "have a transition to itself" in {
    import joos.automata.NonAcceptingNfaNode
    val node = NonAcceptingNfaNode()
    node.addTransition(CharacterA, node).addTransition(CharacterA, NonAcceptingNfaNode())

    node.followTransition(CharacterA) should have size 2
    node.followTransition(CharacterA) should contain(node)
  }

  "A non accepting node" should "be labelled as non accepting" in {
    import joos.automata.NonAcceptingNfaNode
    val node = NonAcceptingNfaNode()

    node.isAccepting() should be(None)
  }

  "An accepting node" should "be labelled as accepting" in {
    import joos.automata.AcceptingNfaNode
    val token = "public"
    val node = AcceptingNfaNode(token)

    node.isAccepting() should be(Some(token))
  }

  "An epsilon closure involving loops" should "return all unique nodes that are part of the epsilon closure" in {
    import joos.automata.{AcceptingNfaNode, NonAcceptingNfaNode}
    val node = NonAcceptingNfaNode()
    node.addTransition(Epsilon, NonAcceptingNfaNode()).
      addTransition(Epsilon, AcceptingNfaNode().addTransition(Epsilon, NonAcceptingNfaNode())).
      addTransition(Epsilon, NonAcceptingNfaNode().addTransition(Epsilon, node))

    node.getClosure(Epsilon) should have size 5
  }

  "An epsilon closure without loops" should "return all unique nodes that are part of the epsilon closure" in {
    import joos.automata.{AcceptingNfaNode, NonAcceptingNfaNode}
    val node = NonAcceptingNfaNode()
    node.addTransition(Epsilon, NonAcceptingNfaNode().addTransition(CharacterA, NonAcceptingNfaNode())).
      addTransition(Epsilon, AcceptingNfaNode().addTransition(Epsilon, NonAcceptingNfaNode()))

    node.getClosure(Epsilon) should have size 4
  }
}
