import joos.a1.SyntaxCheck
import joos.ast.CompilationUnit
import org.scalatest.{Matchers, FlatSpec}

class TemporarySpec extends FlatSpec with Matchers {

  private final val file = getClass.getResource("/a1/marmoset/valid/J1_01.java.test").getPath

  s"${file}" should "be converted to a AST" in {
    val ast = CompilationUnit(SyntaxCheck(file).get.root)
    ast.imports should have length 0
    ast.pkg shouldBe (None)
    ast.typeDeclaration shouldBe Some(null)
   }

}
