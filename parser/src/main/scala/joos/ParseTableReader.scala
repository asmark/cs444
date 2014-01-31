package joos

import scala.io.BufferedSource
import joos.ParseAction.ParseAction

class ParseTableReader(source: BufferedSource) {
  var parseTable = Map[(Int, String), (ParseAction, Int)]()

  val lines = source.getLines().toList
  // Assuming the format is correct
  assert(lines.length > 0)

  var offset = 0
  val num_terminals = lines(offset).toInt
  offset += num_terminals + 1
  val num_non_terminals = lines(offset).toInt
  offset += num_non_terminals + 2
  val num_rules = lines(offset).toInt
  offset += num_rules + 1
  val num_states = lines(offset).toInt
  offset += 1
  val num_actions = lines(offset).toInt
  offset += 1

  for (idx <- offset to offset + num_actions - 1) {
    val line = lines(idx)
    val tokens = line.split(" ")
    assert(tokens.length == 4)
    parseTable += ((tokens(0).toInt, tokens(1)) -> (tokens(2) match {
      case "shift" => ParseAction.Shift
      case "reduce" => ParseAction.Reduce
      case _ => throw new Exception("Invalid action line: " + line)
    }, tokens(3).toInt))
  }

  def getParseTable = parseTable

  def getAction(state: Int, token: String): (ParseAction, Int) = {
    parseTable((state, token))
  }
}

object ParseAction extends Enumeration {
  type ParseAction = Value
  val Reduce, Shift = Value
}

object ParseTableReader {
  def apply(source: BufferedSource) = {
    new ParseTableReader(source)
  }
}
