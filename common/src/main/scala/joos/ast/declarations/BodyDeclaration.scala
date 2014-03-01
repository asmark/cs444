package joos.ast.declarations

import joos.ast.{Modifier, AstNode}
import joos.parsetree.ParseTreeNode

trait BodyDeclaration extends AstNode {
   val modifiers: Seq[Modifier]
}
