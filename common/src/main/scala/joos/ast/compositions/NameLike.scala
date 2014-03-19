package joos.ast.compositions


trait NameLike {

  /**
   * Gets the standard name separated by .
   */
  def standardName: String

  override def toString = standardName

  override def hashCode = standardName.hashCode

  override def equals(that: Any) = that match {
    case that: NameLike => that.standardName == standardName
    case _ => false
  }
}
