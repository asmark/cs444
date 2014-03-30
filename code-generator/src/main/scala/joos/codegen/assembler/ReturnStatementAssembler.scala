package joos.codegen.assembler

import joos.ast.statements.ReturnStatement
import joos.assemgen._

class ReturnStatementAssembler(statement: ReturnStatement) extends Assembler {
  override def generateAssembly() {
    Seq(ret())
  }
}
