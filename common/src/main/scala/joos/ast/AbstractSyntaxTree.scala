package joos.ast

import joos.parsetree.ParseTree
import joos.semantic.ModuleEnvironment

class AbstractSyntaxTree(val root: CompilationUnit)

object AbstractSyntaxTree {
  def apply(parseTree: ParseTree)(implicit moduleEnvironment: ModuleEnvironment): AbstractSyntaxTree = {
    return new AbstractSyntaxTree(CompilationUnit(parseTree.root))
  }
}
