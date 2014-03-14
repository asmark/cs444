package joos.semantic.types.disambiguation

import joos.ast.compositions.LikeName._
import joos.ast.declarations._
import joos.ast.expressions._
import joos.ast.types.PrimitiveType
import joos.ast.types.{ArrayType, SimpleType, Type}
import joos.ast.visitor.AstCompleteVisitor
import joos.core.Logger

class NameClassifier extends AstCompleteVisitor {

  private def classifyNameAs(classification: NameClassification, nameExpression: NameExpression) {
    nameExpression match {
      case QualifiedNameExpression(qualifier, name) => {
        name.classifyContext(classification)
        nameExpression.classifyContext(classification)
        classifyNameAs(classification, qualifier)
      }
      case name: SimpleNameExpression => {
        name.classifyContext(classification)
      }
    }
  }

  private def classifyTypeAs(classification: NameClassification = TypeName, typed: Type) {
    typed match {
      case SimpleType(QualifiedNameExpression(qualifier, name)) => {
        classifyNameAs(PackageOrTypeName, qualifier)
        classifyNameAs(classification, name)
      }
      case SimpleType(simpleName) => {
        classifyNameAs(classification, simpleName)
      }
      case ArrayType(elementType, dimensions) => classifyTypeAs(classification, elementType)
      case _: PrimitiveType =>
    }
  }


  override def apply(packageDeclaration: PackageDeclaration) {
    classifyNameAs(PackageName, packageDeclaration.name)
  }

  override def apply(importDeclaration: ImportDeclaration) {
    if (!importDeclaration.isOnDemand) {
      classifyNameAs(TypeName, importDeclaration.name)
    } else {
      classifyNameAs(PackageOrTypeName, importDeclaration.name)
    }
  }

  override def apply(typeDeclaration: TypeDeclaration) {
    typeDeclaration.superInterfaces foreach (classifyNameAs(TypeName, _))
    typeDeclaration.superType foreach (classifyNameAs(TypeName, _))

    super.apply(typeDeclaration)
  }

  override def apply(fieldDeclaration: FieldDeclaration) {
    classifyTypeAs(TypeName, fieldDeclaration.variableType)

    super.apply(fieldDeclaration)
  }

  override def apply(methodDeclaration: MethodDeclaration) {
    methodDeclaration.returnType foreach (classifyTypeAs(TypeName, _))

    super.apply(methodDeclaration)
  }

  override def apply(variableDeclaration: SingleVariableDeclaration) {
    classifyTypeAs(TypeName, variableDeclaration.declarationType)

    super.apply(variableDeclaration)
  }

  override def apply(castExpression: CastExpression) {
    classifyTypeAs(TypeName, castExpression.castType)

    super.apply(castExpression)
  }

  override def apply(instanceOfExpression: InstanceOfExpression) {
    classifyTypeAs(TypeName, instanceOfExpression.classType)

    super.apply(instanceOfExpression)
  }

  override def apply(classCreationExpression: ClassInstanceCreationExpression) {
    classifyTypeAs(TypeName, classCreationExpression.classType)

    super.apply(classCreationExpression)
  }

  override def apply(arrayAccess: ArrayAccessExpression) {
    arrayAccess.reference match {
      case reference: NameExpression => classifyNameAs(ExpressionName, reference)
      case _ => {
        Logger.logWarning(s"This array reference was given a ${arrayAccess.reference} instead of a NameExpression")
      }
    }
  }

  override def apply(fieldAccess: FieldAccessExpression) {
    classifyNameAs(ExpressionName, fieldAccess.identifier)

    super.apply(fieldAccess)
  }

  override def apply(invocation: MethodInvocationExpression) {
    invocation.methodName match {
      case QualifiedNameExpression(qualifier, name) => {
        classifyNameAs(Ambiguous, qualifier)
        classifyNameAs(MethodName, name)
      }
      case name: SimpleNameExpression => classifyNameAs(MethodName, name)
    }

    super.apply(invocation)
  }

}
