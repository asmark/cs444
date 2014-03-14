package joos.ast

import joos.core.Enumeration

class Operator(val name: String) extends Operator.Value

/**
 * Contains all operators in Joos 1W grammar
 */
object Operator extends Enumeration {
  type T = Operator

  final val Plus = this + new Operator("+")
  final val Minus = this + new Operator("-")
  final val Multiply = this + new Operator("*")
  final val Divide = this + new Operator("/")
  final val Modulo = this + new Operator("%")
  final val ConditionalAnd = this + new Operator("&&")
  final val ConditionalOr = this + new Operator("||")
  final val Greater = this + new Operator(">")
  final val Less = this + new Operator("<")
  final val Equal = this + new Operator("==")
  final val NotEqual = this + new Operator("!=")
  final val LessOrEqual = this + new Operator("<=")
  final val GreaterOrEqual = this + new Operator(">=")
  final val BitwiseAnd = this + new Operator("&")
  final val BitwiseInclusiveOr = this + new Operator("|")
  final val BitwiseExclusiveOr = this + new Operator("^")
  final val BitwiseNot = this + new Operator("~")
}
