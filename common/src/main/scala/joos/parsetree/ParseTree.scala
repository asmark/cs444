package joos.parsetree

class ParseTree(val root: ParseTreeNode) {

}

object ParseTree {
  def apply(root: ParseTreeNode) = {
    new ParseTree(root)
  }
}