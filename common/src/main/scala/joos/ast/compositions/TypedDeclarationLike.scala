package joos.ast.compositions

import joos.ast.Modifier

/**
 * Represents a declaration that has a type
 */
trait TypedDeclarationLike extends TypedLike with DeclarationLike {
  def modifiers: Seq[Modifier]
}