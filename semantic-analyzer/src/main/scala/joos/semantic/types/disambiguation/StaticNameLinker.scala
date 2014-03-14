package joos.semantic.types.disambiguation

import joos.ast.visitor.{AstEnvironmentVisitor, AstCompleteVisitor}
import joos.ast.CompilationUnit
import joos.ast.declarations.MethodDeclaration
import joos.ast.statements._
import joos.ast.expressions.{QualifiedNameExpression, MethodInvocationExpression, VariableDeclarationExpression}
import joos.semantic.{TypeEnvironment, BlockEnvironment}
import joos.ast.types.{PrimitiveType, ArrayType, Type}

class StaticNameLinker(implicit unit: CompilationUnit) extends AstEnvironmentVisitor {

  def getDeclarationRef(t: Type) {
    t match {
      case ArrayType(x,_) => Some(getDeclarationRef(x))
      case PrimitiveType => None

    }
  }

  // TODO: Only visit statics

  override def apply(invocation: MethodInvocationExpression) {
    // TODO
  }

  override def apply(name: QualifiedNameExpression) {
    val names = name.unfold
    var typeIndex = 1
    val a1 = names(1)

    val localVariable = blockEnvironment.getVariable(a1)
    if (localVariable.isDefined) {
     a1.declarationRef = unit.getVisibleType(localVariable.get.declarationType)

    }
    blockEnvironment.getVariable(a1) match {

    }
  }

}
