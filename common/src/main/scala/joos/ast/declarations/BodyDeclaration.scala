package joos.ast.declarations

import joos.ast.AstNode
import joos.ast.Modifier
import joos.ast.compositions.DeclarationLike

trait BodyDeclaration extends AstNode with DeclarationLike {
  val modifiers: Seq[Modifier]
}
