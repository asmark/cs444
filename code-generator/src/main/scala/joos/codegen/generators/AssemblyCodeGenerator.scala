package joos.codegen.generators

import joos.assemgen.AssemblyLine
import joos.ast.expressions._
import joos.ast.{ExpressionDispatcher, AstNode}
import joos.codegen.AssemblyCodeGeneratorEnvironment
import scala.language.implicitConversions

trait AssemblyCodeGenerator {
  def generate()

  def environment: AssemblyCodeGeneratorEnvironment

  def appendText(lines: AssemblyLine*) {
    environment.assemblyManager.appendData(lines: _*)
  }

  def appendGlobal(lines: AssemblyLine*) {
    environment.assemblyManager.appendGlobal(lines: _*)
  }
}

object AssemblyCodeGenerator {
  implicit def toAssemblyCodeGenerator(expression: Expression)
      (implicit environment: AssemblyCodeGeneratorEnvironment): AssemblyCodeGenerator = {
    var generator: AssemblyCodeGenerator = null
    val creator = new ExpressionDispatcher {
      override def apply(expression: ArrayAccessExpression) {

      }

      override def apply(expression: ArrayCreationExpression): Unit = super.apply(expression)

      override def apply(expression: AssignmentExpression): Unit = super.apply(expression)

      override def apply(expression: BooleanLiteral) = generator = new BooleanLiteralCodeGenerator(expression)

      override def apply(expression: CastExpression): Unit = super.apply(expression)

      override def apply(expression: CharacterLiteral): Unit = super.apply(expression)

      override def apply(expression: ClassInstanceCreationExpression): Unit = super.apply(expression)

      override def apply(expression: FieldAccessExpression): Unit = super.apply(expression)

      override def apply(expression: InfixExpression): Unit = super.apply(expression)

      override def apply(expression: InstanceOfExpression): Unit = super.apply(expression)

      override def apply(expression: IntegerLiteral): Unit = super.apply(expression)

      override def apply(expression: MethodInvocationExpression): Unit = super.apply(expression)

      override def apply(expression: NullLiteral): Unit = super.apply(expression)

      override def apply(expression: ParenthesizedExpression): Unit = super.apply(expression)

      override def apply(expression: PrefixExpression): Unit = super.apply(expression)

      override def apply(expression: VariableDeclarationExpression): Unit = super.apply(expression)

      override def apply(expression: ThisExpression): Unit = super.apply(expression)
    }

    creator(expression)
    generator
  }

  implicit def toAssemblyCodeGenerator(element: AstNode)
      (implicit environment: AssemblyCodeGeneratorEnvironment): AssemblyCodeGenerator = {
    // TODO: implement
    null
  }
}
