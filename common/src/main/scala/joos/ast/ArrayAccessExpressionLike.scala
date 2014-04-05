package joos.ast

/**
 * Represents something that can be an array access
 */
trait ArrayAccessExpressionLike {
  def isArrayAccess: Boolean = false
}
