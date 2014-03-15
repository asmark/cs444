package joos.semantic.types

import joos.ast.declarations.MethodDeclaration
import joos.ast.expressions.{MethodInvocationExpression, QualifiedNameExpression, NameExpression, SimpleNameExpression}
import scala.language.implicitConversions

package object disambiguation {

  def matchMethod(invocation: MethodInvocationExpression, declarations: Seq[MethodDeclaration]): Option[MethodDeclaration] = {
    None
  }

  implicit def fold(names: Seq[SimpleNameExpression]): NameExpression = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

}
