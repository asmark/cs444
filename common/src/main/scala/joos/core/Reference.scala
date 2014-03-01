package joos.core


/**
 * Represents a reference to the actual object
 */
class Reference[T >: Null](val id: Int, manager: ReferenceManager[T]) {
  override def hashCode = id

  override def toString = get.toString

  override def equals(that: Any) = {
    that match {
      case that: Reference[T] => that.id == id
      case _ => false
    }
  }

  /**
   * Returns the actual object that this reference is associated with
   */
  @inline def get: T = manager(this)

  /**
   * Returns the actual object as type {{E}}
   */
  @inline def as[E <: T] = get.asInstanceOf[E]
}
