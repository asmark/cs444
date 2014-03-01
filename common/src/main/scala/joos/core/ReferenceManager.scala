package joos.core

import scala.collection.mutable

trait ReferenceManager[T >: Null] {
  private[this] var nextId = 0
  private[this] val references = mutable.ArrayBuffer.empty[T]

  /**
   * Gets the object associated with this {{reference}}
   */
  @inline private[core] def apply(reference: Reference[T]): T = references(reference.id)

  /**
   * Associates the {{reference}} with the {{value}} of type {{T}}
   */
  @inline private[core] def apply(reference: Reference[T], value: T): this.type = {
    references(reference.id) = value
    this
  }

  /**
   * Creates a new reference that's associated with the default value of type {{T}}
   */
  def apply(): Reference[T] = {
    val reference = new Reference[T](nextId, this)
    references += null
    nextId += 1
    reference
  }
}
