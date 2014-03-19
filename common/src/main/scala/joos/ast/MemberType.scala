package joos.ast

import joos.core.Enumeration

class MemberType(val name: String) extends MemberType.Value

object MemberType extends Enumeration {
  type T = MemberType

  final val Static = this + new MemberType("static")
  final val Instance = this + new MemberType("instance")
  final val Unresolved = this + new MemberType("unresolved")
}
