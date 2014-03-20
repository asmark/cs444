package joos.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

trait Enumeration {
  /**
   * The actual type of enumeration
   */
  type T <: Value

  trait Value extends Ordered[Value] {
    val id = _values.length

    /** Name of this value */
    def name: String

    override def hashCode(): Int = id.##

    override def equals(that: scala.Any): Boolean = that match {
      case that: Value => that.id == id
      case _ => false
    }

    override def toString: String = name

    override def compare(that: Value): Int = id - that.id

  }

  private[this] val _values = ArrayBuffer.empty[T]
  private[this] val nameMap = mutable.HashMap.empty[String, T]

  protected def +(value: T): T = {
    _values += value
    assert(nameMap.put(value.name, value).isEmpty)
    value
  }

  def values: collection.IndexedSeq[T] = _values

  def fromName(name: String): T = {
    nameMap(name)
  }
}
