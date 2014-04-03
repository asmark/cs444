package joos

import java.io.PrintWriter
import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.StringLiteral
import scala.language.implicitConversions

package object assemgen {

  /**
   * Indent the rest of the text by this amount
   */
  class Indentation(val amount: Int) extends PseudoAssemblyLine

  /**
   * Represents an empty line
   */
  class EmptyLine() extends PseudoAssemblyLine

  private[this] final val EmptyLine = new EmptyLine()

  private[this] abstract class AbstractAssemblyLine extends AssemblyLine

  private[this] abstract class AbstractAssemblyExpression extends AssemblyExpression

  private[this] class AnyAssembly(text: String) extends AssemblyLine with AssemblyExpression {
    override def write(writer: PrintWriter) {
      writer.print(text)
    }
  }

  implicit class RichMethodDeclaration(val method: MethodDeclaration) extends AnyVal {
    def uniqueName: String = {
      require(method.typeDeclaration != null)

      if (method.isNative) {
        s"NATIVE${method.typeDeclaration.fullName}.${method.name}"
      } else if (method.isConstructor) {
        s"constructor.${method.typeDeclaration.fullName}.${method.id}"
      } else {
        s"method.${method.typeDeclaration.fullName}.${method.name}.${method.id}"
      }
    }
  }

  implicit class RichFieldDeclaration(val field: FieldDeclaration) extends AnyVal {
    def uniqueName: String = {
      require(field.typeDeclaration != null)

      s"field.${field.typeDeclaration.fullName}.${field.declarationName}.${field.id}"
    }
  }

  implicit class RichStringLiteral(val literal: StringLiteral) extends AnyVal {
    def uniqueName: String = {
      s"string.literal.${literal.id}"
    }
  }

  implicit class RichTypeDeclaration(val tipe: TypeDeclaration) extends AnyVal {
    def uniqueName: String = {
      s"type.${tipe.fullName}.${tipe.id}"
    }
  }


  /**
   * Writes any arbitrary expression
   */
  def anyExpression(expression: String): AssemblyExpression = {
    new AbstractAssemblyExpression {
      override def write(writer: PrintWriter) {
        writer.print(expression)
      }
    }
  }

  implicit class RichString(val string: String) extends AnyVal {
    /**
     * Creates a label
     */
    def :: : AssemblyLabel = {
      new AssemblyLabel(string, None)
    }

    def ::(expression: AssemblyExpression): AssemblyLabel = {
      new AssemblyLabel(string, Some(expression))
    }

  }

  /**
   * Increments the indentation by the given {{amount}}
   */
  def #>(amount: Int): Indentation = new Indentation(amount)

  def #> : Indentation = #>(4)

  /**
   * Decrements the indentation by the given {{amount}}
   */
  def #<(amount: Int): Indentation = new Indentation(-amount)

  def #< : Indentation = #<(4)

  /**
   * Writes a comment
   */
  def :#(comment: String): AssemblyComment = {
    new AssemblyComment(comment)
  }

  def mov(destination: AssemblyExpression, source: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("mov", Seq(destination, source), None)
  }

  def movdw(destination: AssemblyExpression, source: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("mov dword", Seq(destination, source), None)
  }

  def movdw(destination: AssemblyExpression, source: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("mov dword", Seq(destination, source), None)
  }

  /**
   * destination = source representing an address
   */
  def lea(destination: AssemblyExpression, source: MemoryReference): AssemblyInstruction = {
    new AssemblyInstruction("lea", Seq(destination, source))
  }

  def inc(reg: Register): AssemblyInstruction = {
    new AssemblyInstruction("inc", Seq(reg))
  }

  def dec(reg: Register): AssemblyInstruction = {
    new AssemblyInstruction("dec", Seq(reg))
  }

  /**
   * eax += ebx
   */
  def add(reg: Register, exp: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("add", Seq(reg, exp))
  }

  /**
   * eax = r1 & r2
   */
  def and(r1: Register, r2: Register): AssemblyInstruction = {
    new AssemblyInstruction("and", Seq(r1, r2))
  }

  /**
   * eax = r1 | r2
   */
  def or(r1: Register, r2: Register): AssemblyInstruction = {
    new AssemblyInstruction("or", Seq(r1, r2))
  }

  /**
   * left -= right
   */
  def sub(left: Register, right: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("sub", Seq(left, right))
  }

  /**
   * Signed multiplication
   * eax *= ebx
   */
  def imul(eax: Register, ebx: Register): AssemblyInstruction = {
    new AssemblyInstruction("imul", Seq(eax, ebx))
  }

  /**
   * Signed multiplication
   * eax = ebx * constant
   */
  def imul(eax: Register, ebx: Register, constant: Int): AssemblyInstruction = {
    new AssemblyInstruction("imul", Seq(eax, ebx, constant))
  }

  /**
   * Signed division
   * eax = edx:eax / ebx
   * edx = edx:eax % ebx
   * Set edx to sign of {{dividend}}
   */
  def idiv(dividend: Register): AssemblyInstruction = {
    new AssemblyInstruction("idiv", Seq(dividend))
  }

  /**
   * eax = -{{target}}
   */
  def neg(target: Register): AssemblyInstruction = {
    new AssemblyInstruction("neg", Seq(target))
  }

  def xor(left: AssemblyExpression, right: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("xor", Seq(left, right))
  }

  /**
   * eip = {{label}}
   */
  def jmp(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jmp", Seq(label))
  }

  /**
   * Prepares {{left}} and {{right}} for conditional jumps
   */
  def cmp(left: AssemblyExpression, right: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("cmp", Seq(left, right))
  }

  /**
   * {{ == }}
   */
  def je(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("je", Seq(label))
  }

  /**
   * {{ != }}
   */
  def jne(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jne", Seq(label))
  }

  /**
   * Signed {{ > }}
   */
  def jg(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jg", Seq(label))
  }

  /**
   * Signed {{ < }}
   */
  def jl(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jl", Seq(label))
  }

  /**
   * Signed {{ >= }}
   */
  def jge(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jge", Seq(label))
  }

  /**
   * Signed {{ <= }}
   */
  def jle(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jle", Seq(label))
  }

  /**
   * Unsigned {{ > }}
   */
  def ja(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("ja", Seq(label))
  }

  /**
   * Unsigned {{ < }}
   */
  def jb(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jb", Seq(label))
  }

  /**
   * Unsigned {{ >= }}
   */
  def jae(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jae", Seq(label))
  }

  /**
   * Unsigned {{ <= }}
   */
  def jbe(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("jbe", Seq(label))
  }

  /**
   * Copies the value of {{eax}} to stack pointer
   * Increments the stack pointer
   */
  def push(eax: Register): AssemblyInstruction = {
    new AssemblyInstruction("push", Seq(eax))
  }

  /**
   * Copies the value pointed by stack pointer to {{eax}}
   * Decrements the stack pointer
   */
  def pop(eax: Register): AssemblyInstruction = {
    new AssemblyInstruction("pop", Seq(eax))
  }

  /**
   * Pushes program counter onto the stack
   * Jumps to the label
   */
  def call(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("call", Seq(label))
  }

  /**
   * Pushes program counter onto the stack
   * Jumps to the address referenced in register
   */
  def call(reg: Register): AssemblyInstruction = {
    new AssemblyInstruction("call", Seq(reg))
  }

  /**
   * Pops the program counter from the stack
   */
  def ret(): AssemblyInstruction = {
    new AssemblyInstruction("ret", Seq())
  }

  /**
   * Same as
   * mov esp,ebp
   * pop ebp
   */
  def leave(): AssemblyInstruction = {
    new AssemblyInstruction("leave")
  }

  /**
   * System call
   */
  def int(address: Int): AssemblyInstruction = {
    new AssemblyInstruction("int", Seq(address))
  }

  /**
   * 4-byte data
   */
  def dd(value: Int): AssemblyInstruction = {
    new AssemblyInstruction("dd", Seq(value))
  }

  def dd(label: LabelReference): AssemblyInstruction = {
    new AssemblyInstruction("dd", Seq(label))
  }

  /**
   * 2-byte data
   */
  def dw(value: String): AssemblyInstruction = {
    new AssemblyInstruction("dw", Seq(toExpression(value)))
  }

  /**
   * 1-byte data
   */
  def db(value: String): AssemblyInstruction = {
    new AssemblyInstruction("db", Seq(toExpression(value)))
  }

  /**
   * Refers to some content in memory at {{address}}
   */
  def at(address: AssemblyExpression): MemoryReference = {
    new MemoryReference(address)
  }

  /**
   * Writes any arbitrary line
   */
  def anyLine(line: String): AssemblyLine = {
    new AnyAssembly(line)
  }

  /**
   * Writes an empty line
   */
  def emptyLine: AssemblyLine = {
    EmptyLine
  }

  /**
   * Defines a section
   */
  def section(section: AssemblySection): AssemblyLine = {
    new AnyAssembly("section ." + section.name)
  }

  /**
   * Exports the {{label}}
   */
  def global(label: LabelReference): AssemblyLine = {
    new AnyAssembly("global " + label.name)
  }

  /**
   * Imports the {{label}}
   */
  def extern(label: LabelReference): AssemblyLine = {
    new AnyAssembly("extern " + label.name)
  }

  implicit def toExpression(value: String): AssemblyExpression = {
    new AnyAssembly('\'' + value + '\'')
  }

  implicit def toExpression(value: Int): AssemblyExpression = {
    new AnyAssembly(value.toString)
  }

  /**
   * Refers to a label
   */
  implicit def labelReference(name: String): LabelReference = {
    new LabelReference(name)
  }
}
