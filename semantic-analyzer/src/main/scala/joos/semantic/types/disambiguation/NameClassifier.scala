package joos.semantic.types.disambiguation

import joos.ast.NameClassification._
import joos.ast.expressions._
import joos.ast.visitor.AbstractSyntaxTreeVisitorBuilder
import joos.ast.{AbstractSyntaxTreeVisitorWithEnvironment, CompilationUnit}
import joos.semantic.BlockEnvironment

/**
 * - Classifies all names
 * - Links type declarations
 */
class NameClassifier(implicit val unit: CompilationUnit)
    extends AbstractSyntaxTreeVisitorWithEnvironment {

  private[this] class NameExpressionClassifier(block: BlockEnvironment) {
    def apply(name: NameExpression) {
      name match {
        case name: SimpleNameExpression => this(name)
        case name: QualifiedNameExpression => this(name)
      }
    }

    private[this] def apply(name: QualifiedNameExpression) {
      this(name.qualifier)
      name.qualifier.nameClassification match {
        case TypeName =>
//          name.name.nameClassification = StaticFieldName
          name.nameClassification = StaticFieldName
        case StaticFieldName | InstanceFieldName | LocalVariableName =>
//          name.name.nameClassification = InstanceFieldName
          name.nameClassification = InstanceFieldName
        case PackageName =>
          unit.getVisibleType(name) match {
            case Some(tipe) =>
//              name.name.nameClassification = TypeName
              name.nameClassification = TypeName
              name.declaration = tipe
            case None =>
//              name.name.nameClassification = PackageName
              name.nameClassification = PackageName
          }
        case _ => throw new AmbiguousNameException(name)
      }
    }

    private[this] def apply(name: SimpleNameExpression) {
      this.block.getLocalVariable(name) match {
        case Some(variable) => name.nameClassification = LocalVariableName; return
        case None =>
      }

      unit.typeDeclaration.get.containedFields.get(name) match {
        case Some(field) => name.nameClassification = InstanceFieldName; return
        case None =>
      }

      unit.getVisibleType(name) match {
        case Some(tipe) =>
          name.nameClassification = TypeName
          name.declaration = tipe
        case None => name.nameClassification = PackageName
      }
    }
  }

  override def apply(fieldAccess: FieldAccessExpression) {
    fieldAccess.identifier.nameClassification = InstanceFieldName
    super.apply(fieldAccess)
  }

  override def apply(invocation: MethodInvocationExpression) {
    invocation.methodName match {
      case name: SimpleNameExpression => name.nameClassification = InstanceMethodName
      case name: QualifiedNameExpression =>
        new NameExpressionClassifier(block)(name.qualifier)
        name.qualifier.nameClassification match {
          case TypeName =>
//            name.name.nameClassification = StaticMethodName
            name.nameClassification = StaticMethodName
          case InstanceFieldName | StaticFieldName | LocalVariableName =>
//            name.name.nameClassification = InstanceMethodName
            name.nameClassification = InstanceMethodName
          case _ => throw new AmbiguousNameException(name)
        }
    }

    super.apply(invocation)
  }
}

object NameClassifier extends AbstractSyntaxTreeVisitorBuilder[NameClassifier] {
  override def build(implicit unit: CompilationUnit) = new NameClassifier
}
