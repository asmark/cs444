package joos.ast

import joos.ast.compositions.DeclarationLike
import joos.core.MutableLink

/**
 * Represents a reference to some declaration of type {{T}}
 */
trait DeclarationReference[T <: DeclarationLike] {
  /**
   * Link to some declaration
   */
  def declarationLink: MutableLink[T]
}
