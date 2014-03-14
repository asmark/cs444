package joos.ast.declarations

import joos.ast.AstNode
import joos.ast.Modifier

trait BodyDeclaration extends AstNode {
  val modifiers: Seq[Modifier]
}
