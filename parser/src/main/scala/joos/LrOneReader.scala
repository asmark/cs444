package joos

import java.io.{InputStreamReader, BufferedReader, InputStream}
import joos.language.ProductionRule
import scala.collection.mutable

class LrOneReader(source: InputStream) {
  private val reader = new BufferedReader(new InputStreamReader(source));

  val numTerminals = reader.readLine().toInt
  val terminals = Range(0, numTerminals).map(i => reader.readLine()).toSet
  assert(terminals.size == numTerminals)

  val numNonTerminals = reader.readLine().toInt
  val nonTerminals = Range(0, numNonTerminals).map(i => reader.readLine()).toSet
  assert(nonTerminals.size == numNonTerminals)

  val startSymbol = reader.readLine()
  assert(nonTerminals.contains(startSymbol))

  val numProductionRules = reader.readLine().toInt
  val productionRules = Range(0, numProductionRules).map {
    idx =>
      val rule = reader.readLine().split(" ", 2)
      assert(rule.length == 2)
      ProductionRule(rule(0), rule(1).split(" "))
  }.toVector
  assert(productionRules.size == numProductionRules)

  val numStates = reader.readLine().toInt

  val numActions = reader.readLine().toInt
  val parseActions = Range(0, numActions).foldRight(mutable.HashMap.empty[Tuple2[Int, String], ParseAction]) {
    (idx, map) =>
      val actionData = reader.readLine().split(" ")
      assert(actionData.length == 4)

      val action = actionData(2) match {
        case "shift" => ShiftAction(actionData(0).toInt, actionData(1), actionData(3).toInt)
        case "reduce" => ReduceAction(actionData(0).toInt, actionData(1), productionRules(actionData(3).toInt))
      }
      map += Tuple2((action.startState, action.trigger), action)
  }.toMap
  assert(parseActions.size == numActions)
  assert(!reader.ready())

  def actionTable = new LrOneActionTable(startSymbol, parseActions)
}

object LrOneReader {
  def apply(source: InputStream) = {
    new LrOneReader(source)
  }
}
