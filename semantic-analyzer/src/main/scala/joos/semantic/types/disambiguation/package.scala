package joos.semantic.types

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions._
import scala.language.implicitConversions
import scala.Some

package object disambiguation {

  def findMethod(targetMethod: SimpleNameExpression, parameters: Seq[Expression], methods: Map[SimpleNameExpression, Set[MethodDeclaration]]): Option[MethodDeclaration] = {
    methods.get(targetMethod) match {
      case None => None
      case Some(ms) => findMethod(targetMethod, parameters, ms)
    }
  }

  def findMethod(targetMethod: SimpleNameExpression, parameters: Seq[Expression], methods: Set[MethodDeclaration]): Option[MethodDeclaration] = {
    methods.find(method => isMatch(method, (targetMethod, parameters)))
  }

  private[this] def isMatch(methodA: MethodDeclaration, targetMethod :(SimpleNameExpression, Seq[Expression])): Boolean = {
    if (methodA.parameters.length != targetMethod._2.length) return false
    for (i <- 0 until methodA.parameters.length) {
      if (methodA.parameters(i).variableType != targetMethod._2(i).declarationType) return false
    }
    true
  }

  implicit def fold(names: Seq[SimpleNameExpression]): NameExpression = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

}
