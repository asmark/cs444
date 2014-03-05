package joos.ast.declarations

import joos.ast.{Modifier, AstNode}

trait BodyDeclaration extends AstNode {
   val modifiers: Seq[Modifier]
}
