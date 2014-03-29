package joos.assembly

import org.scalatest.{Matchers, FlatSpec}
import joos.assembly._
import joos.assembly.Register._
import joos.assembly.AssemblySection._
import java.io.{FileWriter, BufferedWriter, PrintWriter}

class PackageSpec extends FlatSpec with Matchers {

  "Each instruction" should "be correctly formatted" in {
    val writer = new PrintWriter(new BufferedWriter(new FileWriter("instruction.out")))

    val instructions = Seq(
      section(Data),
      label("str"),
      db("Hello world!"),
      emptyLine(),
      section(Text),
      extern("label.2"),
      global("_start"),
      label("_start"),
      comment("this is comment"),
      label("label.1"),
      jmp("label.1"),
      mov(Eax, 4),
      mov(Ebx, 1),
      mov(Ecx, labelReference("str")),
      int(0x80),
      push(Eax),
      pop(Eax),
      add(Ecx, Eax),
      idiv(Edx)
    )

    for (instruction <- instructions) {
      instruction.write(writer)
    }

    writer.flush()
    writer.close()
  }
}
