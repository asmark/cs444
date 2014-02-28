package joos.semantic

import joos.ast.TypedDeclaration
import joos.ast.expressions.NameExpression

/**
 * This class is immutable since it needs to take a snapshot of the environment
 * A later variable declaration in the outer scope should not be visible inside an
 * early inner scope
 */
class BlockEnvironment private(
    val parentEnvironment: Option[EnvironmentWithVariable],
    val variables: Map[NameExpression, TypedDeclaration]) extends EnvironmentWithVariable {

  def add(variable: TypedDeclaration): BlockEnvironment = {
    if (variables.contains(variable.declarationName)) {
      throw new DuplicatedDeclarationException(variable.declarationName)
    }
    new BlockEnvironment(parentEnvironment, variables + ((variable.declarationName, variable)))
  }
}

object BlockEnvironment {
  def apply(parentEnvironment: Option[EnvironmentWithVariable] = None): BlockEnvironment = {
    new BlockEnvironment(parentEnvironment, Map())
  }
}
