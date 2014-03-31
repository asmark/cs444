package joos.codegen

import joos.assemgen.Register._
import joos.assemgen._
import joos.ast.expressions.IntegerLiteral

class IntegerLiteralCodeGenerator(literal: IntegerLiteral)
    (implicit environment: AssemblyCodeGeneratorEnvironment)
    extends AssemblyCodeGenerator {

  override def generate() {
    val assemblyManager = environment.assemblyManager
    assemblyManager.text = assemblyManager.text :+ mov(Eax, literal.value, s"Assemble integer literal: ${literal}")
  }
}
