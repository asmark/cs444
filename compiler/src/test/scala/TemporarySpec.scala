import joos.a1.SyntaxCheck
import joos.ast.CompilationUnit
import org.scalatest.{Matchers, FlatSpec}

class TemporarySpec extends FlatSpec with Matchers {

  private final val file = "/a1/marmoset/valid/J1_01.java.test"

  s"${file}" should "be converted to a AST" ignore {
    CompilationUnit(SyntaxCheck(file).get.root)
  }

}
