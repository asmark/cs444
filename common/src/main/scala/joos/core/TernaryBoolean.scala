package joos.core

import TernaryBoolean._

class TernaryBoolean(val name: String) extends TernaryBoolean.Value {

  def |(that: TernaryBoolean): TernaryBoolean = {
    (this, that) match {
      case (False, False) => False
      case (_, Maybe) | (Maybe, _) => Maybe
      case (True, True) => True
    }
  }
}

object TernaryBoolean extends Enumeration {
  type T = TernaryBoolean

  final val True = this + new TernaryBoolean("true")
  final val False = this + new TernaryBoolean("false")
  final val Maybe = this + new TernaryBoolean("maybe")
}
