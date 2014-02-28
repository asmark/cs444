package joos.ast.expressions

import org.scalatest.{Matchers, FlatSpec}
import scala.collection.mutable

class NameExpressionSpec extends FlatSpec with Matchers {
  it should "have structure equality" in {
    val name1 = NameExpression("abc")
    val name2 = NameExpression("abc.def")
    val name3 = NameExpression("abc.def.ghi")
    val name4 = NameExpression("abc.def.ghi")
    val names = Array(name1, name2, name3, name4)
    val set = mutable.Set[NameExpression]()
    names.foreach(name => set.contains(name) shouldEqual false)
    names.foreach(name => set += name)
    set.size shouldEqual 3
    names.foreach(name => set.contains(name) shouldEqual true)
  }
}
