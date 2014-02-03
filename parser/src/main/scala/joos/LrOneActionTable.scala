package joos

import joos.language.ProductionRule
import joos.exceptions.{ShiftException, ReduceException}

class LrOneActionTable(val startSymbol: String, parseActions: Map[Tuple2[Int, String], ParseAction]) {
  def isReduce(state: Int, trigger: String): Boolean = {
    parseActions.get((state, trigger)) match {
      case Some(ReduceAction(startState, trigger, productionRule)) => true
      case _ => false
    }
  }

  def isShift(state: Int, trigger: String): Boolean = {
    parseActions.get((state, trigger)) match {
      case Some(ShiftAction(startState, trigger, endState)) => true
      case _ => false
    }
  }

  def reduce(state: Int, trigger: String): ProductionRule = {
    parseActions.get((state, trigger)) match {
      case Some(ReduceAction(startState, trigger, productionRule)) => productionRule
      case _ => throw new ReduceException(s"Tried to reduce ($state,$trigger) when no action existed")
    }
  }

  def shift(state: Int, trigger: String): Int = {
    parseActions.get((state, trigger)) match {
      case Some(ShiftAction(startState, trigger, endState)) => endState
      case _ => throw new ShiftException(s"Tried to shift ($state,$trigger) when no action existed")
    }
  }
}
