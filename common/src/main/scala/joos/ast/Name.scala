package joos.ast

trait Name {
  /**
   * Gets the standard name separated by .
   */
  def standardName: String

  override def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: Name => that.standardName == standardName
    case _ => false
  }
}
