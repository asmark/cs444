package joos.core

import scala.collection.mutable
import scala.collection.mutable.ArrayBuffer

abstract class Enumeration {
  /**
   * The actual type of enumeration
   */
  type T <: Value

  protected class Value(val name: String) {
    val id = nextId
    nextId += 1

    override def hashCode(): Int = id

    override def equals(that: scala.Any): Boolean = that match {
      case that: Enumeration#Value => that.id == id
      case _ => false
    }

    override def toString: String = name
  }

  private[this] var nextId = 0
  private[this] val _values = ArrayBuffer.empty[T]
  private[this] val nameMap = mutable.HashMap.empty[String, T]

  protected def add(value: T): T = {
    _values += value
    nameMap.put(value.name, value)
    value
  }

  def values: collection.IndexedSeq[T] = _values

  def fromName(name: String): T = {
    nameMap(name)
  }
}
