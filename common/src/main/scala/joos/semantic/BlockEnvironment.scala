package joos.semantic

import joos.ast.compositions.TypedDeclarationLike
import joos.ast.declarations.SingleVariableDeclaration
import joos.ast.expressions.{VariableDeclarationExpression, SimpleNameExpression, NameExpression}

/**
 * This class is immutable since it needs to take a snapshot of the environment
 * A later variable declaration in the outer scope should not be visible inside an
 * early inner scope
 */
class BlockEnvironment private(
    val typeEnvironment: TypeEnvironment,
    val locals: Map[NameExpression, VariableDeclarationExpression],
    val parameters: Map[NameExpression, SingleVariableDeclaration])
{

  def add(parameter: SingleVariableDeclaration): Option[BlockEnvironment] = {
    getVariable(parameter.declarationName) match {
      case None => Some(new BlockEnvironment(typeEnvironment, locals, parameters + (parameter.declarationName -> parameter)))
      case _ => None
    }
  }

  def add(local: VariableDeclarationExpression): Option[BlockEnvironment] = {
    getVariable(local.declarationName) match {
      case None => Some(new BlockEnvironment(typeEnvironment, locals + (local.declarationName -> local), parameters))
      case _ => None
    }
  }

  def getVariableOrField(name: SimpleNameExpression): Option[TypedDeclarationLike] = {
    getVariable(name) match {
      case None => typeEnvironment.containedFields.get(name)
      case x => x
    }
  }

  def getVariable(name: SimpleNameExpression): Option[TypedDeclarationLike] = {
    locals.get(name) match {
      case None => parameters.get(name)
      case x => x
    }
  }

  def contains(name: SimpleNameExpression): Boolean = {
    locals.contains(name) || typeEnvironment.containedFields.contains(name)
  }
}

object BlockEnvironment {
  def apply()(implicit typeEnvironment: TypeEnvironment): BlockEnvironment = {
    new BlockEnvironment(typeEnvironment, Map(), Map())
  }
}
