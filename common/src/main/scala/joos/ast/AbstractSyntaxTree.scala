package joos.ast

import joos.ast.visitor.AstVisitor
import joos.syntax.parsetree.ParseTree

class AbstractSyntaxTree(val root: CompilationUnit) {
  def dispatch(visitor: AstVisitor) = root accept visitor
}

object AbstractSyntaxTree {
  def apply(parseTree: ParseTree): AbstractSyntaxTree = {
    new AbstractSyntaxTree(CompilationUnit(parseTree.root))
  }
}