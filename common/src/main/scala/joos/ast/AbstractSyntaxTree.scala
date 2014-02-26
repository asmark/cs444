package joos.ast

import joos.parsetree.ParseTree

class AbstractSyntaxTree(val root: AstNode)

object AbstractSyntaxTree {
  def apply(parseTree: ParseTree): AbstractSyntaxTree = {
    return new AbstractSyntaxTree(CompilationUnit(parseTree.root))
  }
}