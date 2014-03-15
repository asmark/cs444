package joos.semantic.types

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions.{QualifiedNameExpression, NameExpression, SimpleNameExpression}
import scala.language.implicitConversions

package object disambiguation {

  def findMethod(targetMethod: MethodDeclaration, methods: Map[SimpleNameExpression, Seq[MethodDeclaration]]): Option[MethodDeclaration] = {
    methods.get(targetMethod.name) match {
      case None => None
      case Some(ms) => findMethod(targetMethod, ms)
    }
  }

  def findMethod(targetMethod: MethodDeclaration, methods: Seq[MethodDeclaration]): Option[MethodDeclaration] = {
    methods.find(method => isMatch(method, targetMethod))
  }

  private[this] def isMatch(methodA: MethodDeclaration, methodB: MethodDeclaration): Boolean = {
    if (methodA.returnType != methodB.returnType) return false
    if (methodA.parameters.length != methodB.parameters.length) return false
    for (i <- 0 until methodA.parameters.length) {
      if (methodA.parameters(i) != methodB.parameters(i)) return false
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
