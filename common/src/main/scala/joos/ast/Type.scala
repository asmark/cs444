package joos.ast

import joos.tokens.TerminalToken

trait Type extends AstNode

case class PrimitiveType(primType: TerminalToken) extends Type

case class ArrayType(elementType: Type, dimensions: Int) extends Type

case class SimpleType(name: NameExpression) extends Type

case class QualifiedType(qualifier: Type, name: NameExpression) extends Type