package joos.ast

import joos.ast.compositions.DeclarationLike

/**
 * Represents a reference to some declaration of type {{T}}
 */
trait DeclarationReference[T <: DeclarationLike] {
  /**
   * Declaration it's referring to
   */
  var declaration: T = _
}
