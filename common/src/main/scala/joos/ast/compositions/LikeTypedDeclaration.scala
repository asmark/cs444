package joos.ast.compositions

import joos.ast.expressions.NameExpression
import joos.ast.types.Type

/**
 * Represents a declaration that has a type
 */
trait LikeTypedDeclaration extends LikeTyped with LikeDeclaration