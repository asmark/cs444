package joos.analysis

import joos.ast.statements.Statement
import joos.compiler.CompilationException
import joos.ast.declarations.MethodDeclaration

package object exceptions {

  class StaticAnalysisException(errorMessage: String) extends CompilationException(errorMessage)

  class UnreachableException private(errorMessage: String) extends StaticAnalysisException(errorMessage) {
    def this(statement: Statement) = this(s"${statement} is unreachable")
  }

  class MissingReturnException private(errorMessage: String) extends StaticAnalysisException(errorMessage) {
    def this(method: MethodDeclaration) = this(s"Not all execution path in method ${method.name} have return statement")
  }
}
