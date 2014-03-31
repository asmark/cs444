package joos.codegen.generators
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.{AssemblyCodeGenerator, AssemblyCodeGeneratorEnvironment}
import joos.ast.expressions.InstanceOfExpression
import joos.codegen.AssemblyFileManager

class InstanceOfExpressionCodeGenerator(expression: InstanceOfExpression)(implicit val assemblyManager: AssemblyFileManager) extends Assembler {
  override def generateAssembly(): Unit = {}
}
