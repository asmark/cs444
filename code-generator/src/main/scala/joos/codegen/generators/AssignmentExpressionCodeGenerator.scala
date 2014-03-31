package joos.codegen.generators

import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.expressions.AssignmentExpression

class AssignmentExpressionCodeGenerator(expression: AssignmentExpression)
  (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

    override def generate() {
      expression.left.generate()
      expression.right.generate()
    }

  }