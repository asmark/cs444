package joos.assemgen

import org.scalatest.{Matchers, FlatSpec}
import joos.assemgen._
import joos.assemgen.Register._
import joos.assemgen.AssemblySection._
import java.io.{FileWriter, BufferedWriter, PrintWriter}
import scala.io.Source
import joos.codegen.AssemblyCodeWriter
import com.sun.xml.internal.messaging.saaj.util.ByteOutputStream

class PackageSpec extends FlatSpec with Matchers {

  "Each instruction" should "be correctly formatted" in {
    val instructions : Seq[AssemblyLine] = Seq(
      section(Data),
      "str"::,
      #>,
      db("Hello world!"),
      #<,
      emptyLine,
      section(Text),
      extern("label.2"),
      global("_start"),
      "_start"::,
      :# ("this is comment"),
      "label.1"::,
      #>,
      jmp("label.1"),
      mov(Eax, 4),
      mov(Ebx, 1),
      mov(Ecx, labelReference("str")),
      int(0x80),
      push(Eax),
      pop(Eax),
      add(Ecx, Eax),
      idiv(Edx),
      mov(Ecx, at(1)),
      mov(Ebx, at(Eax)),
      mov(Ebx, at(Ebx)) :# "with comment",
      ("inline.label":: add(Eax, Ebx)) :# "comment"
    )

    val stream = new ByteOutputStream()
    val writer = new AssemblyCodeWriter(new PrintWriter(stream))
    writer.write(instructions: _*).flush()
    val output = stream.toString

    output shouldEqual Source.fromURL(getClass.getResource("/expect/AssemblyLine")).mkString

  }
}
