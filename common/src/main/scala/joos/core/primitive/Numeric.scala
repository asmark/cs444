package joos.core.primitive

trait Numeric[+T] {
  def value: T

  def toLong: Long

  override def toString = value.toString

  def toChar = this.toLong.toChar

  def +(that: Numeric[_]): Numeric[Long] = this.toLong + that.toLong

  def -(that: Numeric[_]): Numeric[Long] = this.toLong - that.toLong

  def *(that: Numeric[_]): Numeric[Long] = this.toLong * that.toLong

  def /(that: Numeric[_]): Numeric[Long] = this.toLong / that.toLong

  def %(that: Numeric[_]): Numeric[Long] = this.toLong % that.toLong

  def >(that: Numeric[_]): Boolean = this.toLong > that.toLong

  def <(that: Numeric[_]): Boolean = this.toLong < that.toLong

  def >=(that: Numeric[_]): Boolean = this.toLong >= that.toLong

  def <=(that: Numeric[_]): Boolean = this.toLong <= that.toLong

  override def equals(that: Any) = {
    that match {
      case that: Numeric[_] => that.toLong == this.toLong
      case _ => false
    }
  }
}
