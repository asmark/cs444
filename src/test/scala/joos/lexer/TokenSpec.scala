package joos.lexer

import org.scalatest._

class TokenSpec extends FlatSpec with Matchers {

  "Keyword Final" should "be tested" in {
    var start = Token.FINAL.entrance
    var end = start.followTransition('f')
    end should not be empty

    start = end.head.followTransition(NFANode.Epsilon).head
    end = start.followTransition('i')
    end should not be empty

    start = end.head.followTransition(NFANode.Epsilon).head
    end = start.followTransition('n')
    end should not be empty

    start = end.head.followTransition(NFANode.Epsilon).head
    end = start.followTransition('a')
    end should not be empty

    start = end.head.followTransition(NFANode.Epsilon).head
    end = start.followTransition('l')
    end should not be empty

    var accept_node = end.head
    accept_node.isAccepting() should be (Some("FINAL"))
  }
}
