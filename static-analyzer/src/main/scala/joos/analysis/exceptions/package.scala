package joos.analysis

import joos.ast.statements.Statement
import joos.compiler.CompilationException

package object exceptions {

  class StaticAnalysisException(errorMessage: String) extends CompilationException(errorMessage)

  class UnreachableException private(errorMessage: String) extends StaticAnalysisException(errorMessage) {
    def this(statement: Statement) = this(s"${statement} is unreachable")
  }

}
