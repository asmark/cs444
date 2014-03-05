package joos.syntax

package object parser {

  class JoosParseException(msg: String) extends RuntimeException(msg)

  class ReduceException(msg: String) extends JoosParseException(msg)

  class ShiftException(msg: String) extends JoosParseException(msg)

}
