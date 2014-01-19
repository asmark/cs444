package joos.lexer

import org.scalatest._

class RegularExpressionSpec extends FlatSpec with Matchers{

  val test_atom1 = new Atom(NonAcceptingNFANode())
  val test_atom2 = new Atom(AcceptingNFANode())
  val test_concat = new Concatenation()
  test_concat.build(Array(test_atom1, test_atom2))

  val test_atom3 = new Atom(NonAcceptingNFANode())
  val test_atom4 = new Atom(AcceptingNFANode())
  val test_alter = new Alternation()
  test_alter.build(Array(test_atom3, test_atom4))

  val test_atom5 = new Atom(NonAcceptingNFANode())
  val test_closure = new Closure()
  test_closure.build(test_atom5)

  "An Atom" should "be able to hold NonAcceptingNFANode with entrance and exit set properly" in {
    var node = new NonAcceptingNFANode()
    var test_atom = new Atom(node)
    test_atom.node should be (node)
    test_atom.entrance should be (test_atom)
    test_atom.exit should be (test_atom)
  }

  it should "be able to hold AcceptingNFANode and update the node property" in {
    var node = new AcceptingNFANode('a')
    var test_atom = new Atom(node)
    test_atom.node should be (node)
    node.isAccepting() should be (Some('a'))
    var node1 = new AcceptingNFANode()
    test_atom.node = node1
    test_atom.node should be (node1)
  }

  "A Concatenation" should "be able to concatenate two different Regular Expressions" in {
    test_concat.entrance should be (test_atom1)
    test_concat.exit should be (test_atom2)

    var test_atom = new Atom(NonAcceptingNFANode())
    var concat = new Concatenation()
    concat.build(Array(test_atom, test_alter))

    concat.entrance should be (test_atom.entrance)
    concat.exit should be (test_alter.exit)
    var left_tail = test_atom.exit.node.followTransition(NFANode.Epsilon)
    left_tail.head should be (test_alter.entrance.node)
  }

  "An Alternation" should "accept two Regular Expressions" in {
    var test_atom = new Atom(NonAcceptingNFANode())
    var alter = new Alternation()
    alter.build(Array(test_closure, test_atom))

    var entrance_children = alter.entrance.node.followTransition(NFANode.Epsilon)
    entrance_children should be (List(test_closure.entrance.node, test_atom.entrance.node))

    test_closure.exit.node.followTransition(NFANode.Epsilon).head should be (alter.exit.node)
    test_atom.exit.node.followTransition(NFANode.Epsilon).head should be (alter.exit.node)
  }

   "A Closure" should "accept one Regular Expression" in {
     var closure = new Closure()
     closure.build(test_alter)

     var entrance_children = closure.entrance.node.followTransition(NFANode.Epsilon)
     assert(entrance_children.length equals 2)
     assert(entrance_children.contains(closure.exit.node))
     assert(entrance_children.contains(test_alter.entrance.node))

     var regexp_children = test_alter.exit.node.followTransition(NFANode.Epsilon)
     assert(regexp_children.contains(test_alter.entrance.node))
     assert(regexp_children.contains(closure.exit.node))
   }
}
