package joos.ast.compositions

import joos.ast.expressions.NameExpression
/**
 * Represents a declaration that has a type
 */
trait LikeTypedDeclaration extends LikeTyped with LikeDeclaration

import joos.ast.types.Type
