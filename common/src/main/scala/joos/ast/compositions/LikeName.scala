package joos.ast.compositions

trait LikeName {
  /**
   * Gets the standard name separated by .
   */
  def standardName: String
  var declaration: LikeDeclaration = null

  override implicit def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: LikeName => that.standardName == standardName
    case _ => false
  }
}
