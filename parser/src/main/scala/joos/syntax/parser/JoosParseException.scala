package joos.syntax.parser

import joos.syntax.JoosSyntaxException

class JoosParseException(msg: String) extends JoosSyntaxException(msg)

class ReduceException(msg: String) extends JoosParseException(msg)

class ShiftException(msg: String) extends JoosParseException(msg)