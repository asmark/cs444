package joos.semantic

import joos.ast.TypedDeclaration
import joos.ast.expressions.NameExpression

trait EnvironmentWithVariable extends Environment {
  protected def parentEnvironment: Option[EnvironmentWithVariable]

  protected def variables: collection.Map[NameExpression, TypedDeclaration]

  def getVariable(name: NameExpression): Option[TypedDeclaration] = {
    variables.get(name) match {
      case None => parentEnvironment match {
        case None => None
        case Some(parent) => parent.getVariable(name)
      }
      case variable => variable
    }
  }
}
