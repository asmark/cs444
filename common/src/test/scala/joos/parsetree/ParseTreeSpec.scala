package joos.parsetree

import org.scalatest.{FlatSpec, Matchers}

class ParseTreeSpec extends FlatSpec with Matchers {

  behavior of "level order"
  it should "output the parse tree in level order" in {
    val parseTree = ParseTree(
      TreeNode(
        "expr", Vector(
          TreeNode("term", Vector(LeafNode("id"))),
          LeafNode("+"),
          TreeNode(
            "term", Vector(LeafNode("id"))
          )
        )
      )
    )

    val levelOrder = parseTree.levelOrder

    levelOrder.length shouldEqual 3
    levelOrder(0) shouldEqual Seq("expr")
    levelOrder(1) shouldEqual Seq("term", "+", "term")
    levelOrder(2) shouldEqual Seq("id", "id")
  }
}
