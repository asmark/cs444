package joos.semantic.types.disambiguation

import joos.ast.NameClassification._
import joos.ast.expressions._
import joos.ast.visitor.AbstractSyntaxTreeVisitorBuilder
import joos.ast.{AbstractSyntaxTreeVisitorWithEnvironment, CompilationUnit}
import joos.semantic.BlockEnvironment
import joos.semantic.types.TypeCheckingException

/**
 * - Classifies all names
 * - Links all type names to its declaration
 * - Links all local variable names to its declarations
 */
class NameClassifier(implicit val unit: CompilationUnit)
    extends AbstractSyntaxTreeVisitorWithEnvironment {

  private[this] class Classifier(block: BlockEnvironment) {
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
          name.nameClassification = StaticFieldName
        case StaticFieldName | InstanceFieldName | LocalVariableName =>
          name.nameClassification = InstanceFieldName
        case PackageName =>
          unit.getVisibleType(name) match {
            case Some(tipe) =>
              name.nameClassification = TypeName
              name.declaration = tipe
              name.expressionType = tipe.asType
            case None =>
              name.nameClassification = PackageName
          }
        case _ => throw new AmbiguousNameException(name)
      }

    }

    private[this] def apply(name: SimpleNameExpression) {
      this.block.getVariable(name) match {
        case Some(variable) =>
          name.nameClassification = LocalVariableName
          name.declaration = variable
          name.expressionType = variable.declarationType
          return
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
          name.expressionType = tipe.asType
        case None => name.nameClassification = PackageName
      }
    }
  }

  override def apply(parenthesis: ParenthesizedExpression) {
    super.apply(parenthesis)

    parenthesis.expression match {
      case name: NameExpression =>
        name.nameClassification match {
          case TypeName | PackageName => throw new TypeCheckingException("()", s"${name} cannot be a type or package inside ()")
          case _ =>
        }
      case _ =>
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
        new Classifier(block)(name.qualifier)
        name.qualifier.nameClassification match {
          case TypeName =>
            name.nameClassification = StaticMethodName
          case InstanceFieldName | StaticFieldName | LocalVariableName =>
            name.nameClassification = InstanceMethodName
          case _ => throw new AmbiguousNameException(name)
        }
    }

    super.apply(invocation)
  }

  override def apply(name: QualifiedNameExpression) {
    new Classifier(block)(name)
  }

  override def apply(name: SimpleNameExpression) {
    new Classifier(block)(name)
  }
}

object NameClassifier extends AbstractSyntaxTreeVisitorBuilder[NameClassifier] {
  override def build(implicit unit: CompilationUnit) = new NameClassifier
}
