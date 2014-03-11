package joos.semantic.types.disambiguation

import joos.ast.expressions.NameExpression
import joos.semantic.types.TypeCheckingException

abstract class DisambiguationException(message: String) extends TypeCheckingException(message)

class ForwardFieldUseException(name: NameExpression) extends DisambiguationException(s"${name.standardName} was used before declaration")