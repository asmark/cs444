package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.NameClassification._
import joos.ast.expressions.{Expression, NameExpression, SimpleNameExpression, QualifiedNameExpression}
import joos.ast.types.{SimpleType, ArrayType, PrimitiveType}

/**
 * - Links field names to its declaration
 * - Links variable names to its declaration
 */
class VariableNameLinker(prefix: Option[Expression], name: NameExpression)(implicit val unit: CompilationUnit) {

  def apply() {
    this(name)
  }

  private[this] def apply(name: NameExpression) {
    name match {
      case name: SimpleNameExpression => this(name)
      case name: QualifiedNameExpression => this(name)
    }
  }

  private[this] def apply(name: QualifiedNameExpression) {

  }

  private[this] def apply(name: SimpleNameExpression) {
    val prefixType = prefix match {
      case None => unit.typeDeclaration.get.asType
      case Some(tipe) => tipe.expressionType
    }

    require(prefixType != null)

    name.nameClassification match {
      case InstanceFieldName =>
        prefixType match {
          case tipe: PrimitiveType => throw new MemberNotFoundException(name)
          case tipe: ArrayType =>
            if (name == ArrayType.Length.declarationName) {
              name.declaration = ArrayType.Length
            } else {
              throw new MemberNotFoundException(name)
            }
          case tipe: SimpleType =>
            require(tipe.declaration != null)
            name.declaration = tipe.declaration
            name.expressionType = tipe.declaration.asType
        }
      case PackageName | TypeName =>
    }
  }
}
