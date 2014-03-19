package joos.semantic.types.disambiguation

import joos.ast.CompilationUnit
import joos.ast.NameClassification._
import joos.ast.expressions.{Expression, NameExpression, SimpleNameExpression, QualifiedNameExpression}
import joos.ast.types.{Type, SimpleType, ArrayType, PrimitiveType}

/**
 * - Links field names to its declaration
 */
class FieldNameLinker(prefix: Option[Expression], name: NameExpression)(implicit val unit: CompilationUnit) {

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
    this(name.qualifier)
    name.nameClassification match {
      case InstanceFieldName | StaticFieldName =>
        link(name.qualifier.expressionType, name.name)
        name.expressionType = name.name.expressionType
        name.declaration = name.name.declaration
      case PackageName | TypeName | LocalVariableName =>
    }
  }

  private[this] def apply(name: SimpleNameExpression) {
    val prefixType = prefix match {
      case None => unit.typeDeclaration.get.asType
      case Some(tipe) => tipe.expressionType
    }

    assert(prefixType != null)

    name.nameClassification match {
      case InstanceFieldName => link(prefixType, name)
      case PackageName | TypeName | LocalVariableName =>
    }
  }

  private[this] def link(prefixType: Type, name: SimpleNameExpression) {
    prefixType match {
      case prefixType: PrimitiveType => throw new MemberNotFoundException(name)
      case prefixType: ArrayType =>
        if (name == ArrayType.Length.declarationName) {
          name.declaration = ArrayType.Length
          name.expressionType = ArrayType.Length.variableType
        } else {
          throw new MemberNotFoundException(name)
        }
      case prefixType: SimpleType =>
        require(prefixType.declaration != null)
        val field = prefixType.declaration.containedFields.get(name) match {
          case None => throw new MemberNotFoundException(name)
          case Some(field) => field
        }
        name.declaration = field
        name.expressionType = field.variableType
      case null =>
    }
  }
}
