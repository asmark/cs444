package joos.parser

import joos.language.ProductionRule

sealed abstract class ParseAction {
  def startState: Integer

  def trigger: String
}

case class ShiftAction(startState: Integer, trigger: String, endState: Integer) extends ParseAction

case class ReduceAction(startState: Integer, trigger: String, rule: ProductionRule) extends ParseAction