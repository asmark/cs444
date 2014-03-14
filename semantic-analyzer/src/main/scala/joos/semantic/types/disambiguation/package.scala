package joos.semantic.types

import joos.ast.CompilationUnit
import joos.ast.declarations.BodyDeclaration
import joos.ast.expressions.{QualifiedNameExpression, NameExpression, SimpleNameExpression}
import joos.ast.types.{PrimitiveType, SimpleType, ArrayType, Type}
import scala.language.implicitConversions
import joos.semantic.Declaration

package object disambiguation {

  def getDeclarationRef(t: Type)(implicit unit: CompilationUnit): Declaration = {

    def getTypeDeclaration(t: Type): Option[BodyDeclaration] = {
      t match {
        case ArrayType(x, _) => getTypeDeclaration(x)
        case SimpleType(x) => unit.getVisibleType(x)
        case _: PrimitiveType => None
      }
    }

    t match {
      case _: ArrayType => Left(getTypeDeclaration(t))
      case _: PrimitiveType => Left(None)
      case _: SimpleType => Right(getTypeDeclaration(t).get)
    }
  }

  implicit def fold(names: Seq[SimpleNameExpression]): NameExpression = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

}
