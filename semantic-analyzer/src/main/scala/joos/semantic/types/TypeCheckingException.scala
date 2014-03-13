package joos.semantic.types

import joos.semantic.SemanticException

class TypeCheckingException(msg: String) extends SemanticException(msg)

// TODO: Refine the exception
class ImplicitThisInStaticException(msg: String) extends TypeCheckingException(msg)

class AbstractClassCreationException(msg: String) extends TypeCheckingException(msg)

class ClassCreationException(msg: String) extends TypeCheckingException(msg)

class ArrayAccessException(msg: String) extends TypeCheckingException(msg)

class ArrayCreationException(msg: String) extends TypeCheckingException(msg)

class PrefixExpressionException(msg: String) extends TypeCheckingException(msg)

class CastExpressionException(msg: String) extends TypeCheckingException(msg)