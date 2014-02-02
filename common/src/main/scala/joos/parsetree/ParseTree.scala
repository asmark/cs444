package joos.parsetree

class ParseTree(val root: ParseTreeNode) {
  def getRoot = root
}

object ParseTree {
  def apply(root: ParseTreeNode) = {
    new ParseTree(root)
  }
}