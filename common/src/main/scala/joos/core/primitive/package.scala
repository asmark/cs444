package joos.core

package object primitive {

  implicit class BoxedLong(val value: Long) extends Numeric[Long] {
    override def toLong = value
  }

  implicit class BoxedChar(val value: Char) extends Numeric[Char] {
    override def toLong = value.toLong
  }

  implicit class BoxedInt(val value: Int) extends Numeric[Int] {
    override def toLong = value.toLong
  }

}
