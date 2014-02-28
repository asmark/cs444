package joos.ast

import joos.parsetree.ParseTree

class AbstractSyntaxTree(val root: CompilationUnit)

object AbstractSyntaxTree {
  def apply(parseTree: ParseTree): AbstractSyntaxTree = {
    return new AbstractSyntaxTree(CompilationUnit(parseTree.root))
  }
}