package joos

import java.io.PrintWriter
import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.StringLiteral
import joos.core.DefaultUniqueIdGenerator
import scala.language.implicitConversions

package object assemgen {

  private[this] final val EmptyLine = new AbstractAssemblyLine {
    override def write(writer: PrintWriter) {
      writer.println()
    }
  }

  private[this] abstract class AbstractAssemblyLine extends AssemblyLine

  private[this] abstract class AbstractAssemblyExpression extends AssemblyExpression

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
  }

  /**
   * Writes a comment
   */
  implicit def #:(comment: String): AssemblyComment = {
    new AssemblyComment(comment)
  }

  def mov(destination: AssemblyExpression, source: AssemblyExpression): AssemblyInstruction = {
    new AssemblyInstruction("mov", Seq(destination, source), None)
  }

  /**
   * eax += ebx
   */
  def add(eax: Register, ebx: Register): AssemblyInstruction = {
    new AssemblyInstruction("add", Seq(eax, ebx))
  }

  /**
   * eax -= ebx
   */
  def sub(eax: Register, ebx: Register): AssemblyInstruction = {
    new AssemblyInstruction("sub", Seq(eax, ebx))
  }

  /**
   * Signed multiplication
   * eax *= ebx
   */
  def imul(eax: Register, ebx: Register): AssemblyInstruction = {
    new AssemblyInstruction("imul", Seq(eax, ebx))
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
   * Prepares {{eax}} and {{ebx}} for conditional jumps
   */
  def cmp(eax: Register, ebx: Register): AssemblyInstruction = {
    new AssemblyInstruction("cmp", Seq(eax, ebx))
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

//  /**
//   * Writes any arbitrary line
//   */
//  def anyLine(line: String): AssemblyLine = {
//    new AbstractAssemblyLine {
//      override protected def writeContent(writer: PrintWriter) {
//        writer.print(line)
//      }
//    }
//  }

  /**
   * Writes an empty line
   */
  def emptyLine(): AssemblyLine = {
    EmptyLine
  }

  /**
   * Defines a section
   */
  def section(section: AssemblySection): AssemblyLine = {
    new AbstractAssemblyLine {
      /**
       * Writes the content of this line to the writer
       */
      override def write(writer: PrintWriter) {
        writer.print("section ")
        section.write(writer)
      }
    }
  }

  /**
   * Exports the {{label}}
   */
  def global(label: LabelReference): AssemblyLine = {
    new AbstractAssemblyLine {
      override def write(writer: PrintWriter) {
        writer.print("global ")
        label.write(writer)
      }
    }
  }

  /**
   * Imports the {{label}}
   */
  def extern(label: LabelReference): AssemblyLine = {
    new AbstractAssemblyLine {
      override def write(writer: PrintWriter) {
        writer.print("extern ")
        label.write(writer)
      }
    }
  }

  /**
   * Defines a label
   */
  def label(name: String): AssemblyLabel = {
    name.::
  }
//
//  def inlineLabel(name: String, line: AssemblyLine): AssemblyLine = {
//    new AbstractAssemblyLine {
//
//      override def write(writer: PrintWriter) {
//        writeContent(writer)
//      }
//
//      override protected def writeContent(writer: PrintWriter) {
//        writer.print(name)
//        writer.print(": ")
//        line.write(writer)
//      }
//    }
//  }

  implicit def toExpression(value: String): AssemblyExpression = {
    new AbstractAssemblyExpression {
      override def write(writer: PrintWriter) {
        writer.print('\'')
        writer.print(value)
        writer.print('\'')
      }
    }
  }

  implicit def toExpression(value: Int): AssemblyExpression = {
    new AbstractAssemblyExpression {
      override def write(writer: PrintWriter) {
        writer.print(value)
      }
    }
  }

  implicit def toExpression(value: Long): AssemblyExpression = {
    new AbstractAssemblyExpression {
      override def write(writer: PrintWriter) {
        writer.print(value)
      }
    }
  }

  /**
   * Refers to a label
   */
  implicit def labelReference(name: String): LabelReference = {
    new LabelReference(name)
  }


}
