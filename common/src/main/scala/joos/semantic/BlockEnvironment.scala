package joos.semantic

import joos.ast.compositions.TypedDeclarationLike
import joos.ast.expressions.{SimpleNameExpression, NameExpression}

/**
 * This class is immutable since it needs to take a snapshot of the environment
 * A later variable declaration in the outer scope should not be visible inside an
 * early inner scope
 */
class BlockEnvironment private(
    val typeEnvironment: TypeEnvironment,
    val variables: Map[NameExpression, TypedDeclarationLike]) {

  def add(variable: TypedDeclarationLike) = {
    variables.contains(variable.declarationName) match {
      case true => None
      case false => Some(new BlockEnvironment(typeEnvironment, variables + ((variable.declarationName, variable))))
    }
  }

  def getVariable(name: SimpleNameExpression): Option[TypedDeclarationLike] = {
    variables.get(name) match {
      case None => typeEnvironment.containedFields.get(name)
      case x => x
    }
  }
}

object BlockEnvironment {
  def apply()(implicit typeEnvironment: TypeEnvironment): BlockEnvironment = {
    new BlockEnvironment(typeEnvironment, Map())
  }
}
