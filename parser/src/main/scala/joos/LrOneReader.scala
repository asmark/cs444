package joos

import java.io.{InputStreamReader, BufferedReader, InputStream}
import joos.core._
import joos.language.ContextFreeGrammar
import scala.collection.mutable

object LrOneReader {
  def apply(lalr1TableStream: InputStream, contextFreeGrammarStream: InputStream) = {
    val grammar = ContextFreeGrammar.deserialize(contextFreeGrammarStream)

    using(new BufferedReader(new InputStreamReader(lalr1TableStream))) {
      reader =>
        val numStates = reader.readLine().toInt
        val numActions = reader.readLine().toInt
        val parseActions = Range(0, numActions).foldRight(mutable.HashMap.empty[(Int, String), ParseAction]) {
          (idx, map) =>
            val actionData = reader.readLine().split(" ")
            assert(actionData.length == 4)

            val action = actionData(2) match {
              case "shift" => ShiftAction(actionData(0).toInt, actionData(1), actionData(3).toInt)
              case "reduce" => ReduceAction(actionData(0).toInt, actionData(1), grammar.rules(actionData(3).toInt))
            }
            map += (((action.startState, action.trigger), action))
        }
        assert(parseActions.size == numActions)
        assert(!reader.ready())
        new LrOneActionTable(grammar.start, numStates, parseActions)
    }
  }
}
