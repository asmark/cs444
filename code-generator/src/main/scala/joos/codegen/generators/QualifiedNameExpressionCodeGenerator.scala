package joos.codegen.generators

import joos.ast.expressions.QualifiedNameExpression
import joos.codegen.AssemblyFileManager
import joos.assemgen.Register._
import joos.assemgen._
import joos.codegen.AssemblyCodeGeneratorEnvironment
import joos.ast.NameClassification._
import joos.core.Logger

class QualifiedNameExpressionCodeGenerator(expression: QualifiedNameExpression)
    (implicit val environment: AssemblyCodeGeneratorEnvironment) extends AssemblyCodeGenerator {

  override def generate() {

    expression.nameClassification match {
      case InstanceFieldName => {
        
      }

      case e =>
        Logger.logWarning(s"No support for accessing ${expression} of classification ${e}")
    }

  }

}
