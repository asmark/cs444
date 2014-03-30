package joos.ast

import joos.ast.visitor.AstVisitor
import joos.syntax.parsetree.ParseTree
import java.io.{File, PrintWriter}

class AbstractSyntaxTree(val root: CompilationUnit) {
  lazy val name = {
    val className = root.typeDeclaration match {
      case None => ""
      case Some(typeDeclaration) => typeDeclaration.name.standardName
    }
    root.packageDeclaration.name.standardName match {
      case "" => className
      case packageName => s"${packageName}.${className}"
    }
  }

  lazy val codeStream = new PrintWriter(new File(s"out/${name}}.s"))

  def dispatch(visitor: AstVisitor) = root accept visitor
}

object AbstractSyntaxTree {
  def apply(parseTree: ParseTree): AbstractSyntaxTree = {
    new AbstractSyntaxTree(CompilationUnit(parseTree.root))
  }
}