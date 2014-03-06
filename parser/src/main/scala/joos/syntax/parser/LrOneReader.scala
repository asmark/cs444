package joos.syntax.parser

import java.io.{InputStreamReader, BufferedReader, InputStream}
import joos.syntax.language.ProductionRule
import scala.collection.mutable

class LrOneReader(source: InputStream) {
  private val reader = new BufferedReader(new InputStreamReader(source))

  val numTerminals = reader.readLine().toInt
  val terminals = Range(0, numTerminals).foldRight(mutable.Set.empty[String])((idx, set) => set += reader.readLine())
  assert(terminals.size == numTerminals)

  val numNonTerminals = reader.readLine().toInt
  val nonTerminals = Range(0, numNonTerminals)
    .foldRight(mutable.Set.empty[String])((idx, set) => set += reader.readLine())
  assert(nonTerminals.size == numNonTerminals)

  val startSymbol = reader.readLine()
  assert(nonTerminals.contains(startSymbol))

  val numProductionRules = reader.readLine().toInt
  val productionRules = Range(0, numProductionRules).map {
    idx =>
      val rule = reader.readLine().split(" ", 2)
      assert(rule.length >= 1)
      rule.length match {
        case 1 => ProductionRule(rule(0), IndexedSeq.empty[String])
        case _ => ProductionRule(rule(0), rule(1).split(" "))
      }
  }
  assert(productionRules.size == numProductionRules)

  val numStates = reader.readLine().toInt

  val numActions = reader.readLine().toInt
  val parseActions = Range(0, numActions).foldRight(mutable.HashMap.empty[(Int, String), ParseAction]) {
    (idx, map) =>
      val actionData = reader.readLine().split(" ")
      assert(actionData.length == 4)

      val action = actionData(2) match {
        case "shift" => ShiftAction(actionData(0).toInt, actionData(1), actionData(3).toInt)
        case "reduce" => ReduceAction(actionData(0).toInt, actionData(1), productionRules(actionData(3).toInt))
      }
      map += (((action.startState, action.trigger), action))
  }
  assert(parseActions.size == numActions)
  assert(!reader.ready())

  def actionTable = new LrOneActionTable(startSymbol, parseActions)
}

object LrOneReader {
  def apply(source: InputStream) = {
    new LrOneReader(source)
  }
}
