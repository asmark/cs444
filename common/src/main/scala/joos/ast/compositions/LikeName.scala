package joos.ast.compositions

trait LikeName {
  /**
   * Gets the standard name separated by .
   */
  def standardName: String

  override implicit def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: LikeName => that.standardName == standardName
    case _ => false
  }
}
