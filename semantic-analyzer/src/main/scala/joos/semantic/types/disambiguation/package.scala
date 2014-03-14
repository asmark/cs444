package joos.semantic.types

import joos.ast.CompilationUnit
import joos.ast.declarations.{FieldDeclaration, MethodDeclaration, TypeDeclaration, BodyDeclaration}
import joos.ast.expressions.{QualifiedNameExpression, NameExpression, SimpleNameExpression}
import joos.ast.types.{PrimitiveType, SimpleType, ArrayType, Type}
import scala.language.implicitConversions
import joos.semantic.Declaration

package object disambiguation {

  def matchMethod(invocation: MethodDeclaration, declarations: Seq[MethodDeclaration]): Option[MethodDeclaration] = {
    None
  }

  def getDeclarationRef(t: Type)(implicit unit: CompilationUnit): Declaration = {

    def getTypeDeclaration(t: Type): Option[BodyDeclaration] = {
      t match {
        case ArrayType(x, _) => getTypeDeclaration(x)
        case SimpleType(x) => unit.getVisibleType(x)
        case _: PrimitiveType => None
      }
    }

    t match {
      case _: ArrayType => (t, getTypeDeclaration(t))
      case _: PrimitiveType => (t, None)
      case _: SimpleType => (t, getTypeDeclaration(t))
    }
  }

  def getDeclarationRef(t: TypeDeclaration): Declaration = {
    (SimpleType(t.name), Some(t))
  }

  def getDeclarationRef(m: MethodDeclaration): Declaration = {
    (m.returnType.get, Some(m))
  }

  def getDeclarationRef(f: FieldDeclaration): Declaration = {
    (f.variableType, Some(f))
  }

  implicit def fold(names: Seq[SimpleNameExpression]): NameExpression = {
    names.reduceLeft {
      (tree: NameExpression, name: SimpleNameExpression) =>
        QualifiedNameExpression(tree, name)
    }
  }

}
