package joos.lexer

import org.scalatest._

class RegularExpressionSpec extends FlatSpec with Matchers{

  val test_atom1 = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), '1')
  val test_atom2 = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), '2')
  val test_concat = test_atom1 + test_atom2

  val test_atom3 = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), '3')
  val test_atom4 = new Atom(NonAcceptingNFANode(), AcceptingNFANode(), '4')
  val test_alter = test_atom3 | test_atom4

  val test_atom5 = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), '5')
  val test_closure = test_atom5*

  "An Atom" should "be able to hold NonAcceptingNFANode with entrance and exit set properly" in {
    val node1 = new NonAcceptingNFANode()
    val node2 = new NonAcceptingNFANode()
    val test_atom = new Atom(node1, node2, 'c')
    test_atom.char should be ('c')
    test_atom.entrance should be (node1)
    test_atom.exit should be (node2)
  }

  it should "be able to hold AcceptingNFANode and update the node property" in {
    val node1 = new NonAcceptingNFANode()
    val node2 = new AcceptingNFANode('a')
    val test_atom = new Atom(node1, node2, 'c')
    test_atom.entrance should be (node1)
    test_atom.exit.isAccepting() should be (Some('a'))
    val node3 = new AcceptingNFANode()
    test_atom.entrance = node3
    test_atom.entrance should be (node3)
  }

  "A Concatenation" should "be able to concatenate two different Regular Expressions" in {
    test_concat.entrance should be (test_atom1.entrance)
    test_concat.exit should be (test_atom2.exit)

    val test_atom = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), 'c')
    val atom_exit = test_atom.exit
    val concat = test_atom + test_alter

    concat.entrance should be (test_atom.entrance)
    concat.exit should be (test_alter.exit)
    val left_tail = atom_exit.followTransition(NFANode.Epsilon)
    left_tail.head should be (test_alter.entrance)
  }

  "An Alternation" should "accept two Regular Expressions" in {
    val test_atom = new Atom(NonAcceptingNFANode(), NonAcceptingNFANode(), 'c')
    val test_closure_entrance = test_closure.entrance
    val test_closure_exit = test_closure.exit
    val alter = test_closure | test_atom

    val entrance_children = alter.entrance.followTransition(NFANode.Epsilon)
    entrance_children should be (List(test_closure_entrance, test_atom.entrance))

    test_closure_exit.followTransition(NFANode.Epsilon).head should be (alter.exit)
    test_atom.exit.followTransition(NFANode.Epsilon).head should be (alter.exit)
  }

   "A Closure" should "accept one Regular Expression" in {
     val inner_entrance = test_alter.entrance
     val inner_exit = test_alter.exit
     val closure = test_alter.*

     val entrance_children = closure.entrance.followTransition(NFANode.Epsilon)
     entrance_children should have length 2
     entrance_children should contain (closure.exit)
     entrance_children should contain (inner_entrance)

     val regexp_children = inner_exit.followTransition(NFANode.Epsilon)
     regexp_children should contain (inner_entrance)
     regexp_children should contain (closure.exit)
   }
}
