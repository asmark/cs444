package joos

import java.io.PrintWriter
import joos.ast.declarations.{TypeDeclaration, FieldDeclaration, MethodDeclaration}
import joos.ast.expressions.StringLiteral
import scala.language.implicitConversions

package object assemgen {

  private[this] final val EmptyLine = new AbstractAssemblyLine {
    override protected def writeContent(writer: PrintWriter) {}
  }

  private[this] abstract class AbstractAssemblyLine extends AssemblyLine

  private[this] abstract class AbstractAssemblyExpression extends AssemblyExpression

  private[this] class InstructionLine(
      instruction: String, operands: Seq[AssemblyExpression], comment: Option[String] = None)
      extends AssemblyLine {

    protected override def writeContent(writer: PrintWriter) {
      writer.print("    ")
      writer.print(instruction)
      writer.print(' ')

      operands.foldLeft(true) {
        (isFirst, operand) =>
          if (isFirst) {
            operand.write(writer)
          } else {
            writer.print(", ")
            operand.write(writer)
          }
          false
      }

      comment match {
        case None =>
        case Some(comment) =>
          writer.print("    ; ")
          writer.print(comment)
      }
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
      s"literal.string.${literal.id}"
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

  /**
   * Writes a comment line
   */
  def comment(comment: String, indentation: Int = 4): AssemblyLine = {
    new AbstractAssemblyLine {
      override protected def writeContent(writer: PrintWriter) {
        for (i <- 0 until indentation) {
          writer.print(' ')
        }
        writer.print("; ")
        writer.print(comment)
      }
    }
  }

  def mov(destination: AssemblyExpression, source: AssemblyExpression, comment: Option[String] = None): AssemblyLine = {
    new InstructionLine("mov", Seq(destination, source), comment)
  }

  def mov(destination: AssemblyExpression, source: AssemblyExpression, comment: String): AssemblyLine = {
    mov(destination, source, Some(comment))
  }

  /**
   * eax += ebx
   */
  def add(eax: Register, ebx: Register): AssemblyLine = {
    new InstructionLine("add", Seq(eax, ebx))
  }

  /**
   * eax -= ebx
   */
  def sub(eax: Register, ebx: Register): AssemblyLine = {
    new InstructionLine("sub", Seq(eax, ebx))
  }

  /**
   * Signed multiplication
   * eax *= ebx
   */
  def imul(eax: Register, ebx: Register): AssemblyLine = {
    new InstructionLine("imul", Seq(eax, ebx))
  }

  /**
   * Signed division
   * eax = edx:eax / ebx
   * edx = edx:eax % ebx
   * Set edx to sign of {{dividend}}
   */
  def idiv(dividend: Register): AssemblyLine = {
    new InstructionLine("idiv", Seq(dividend))
  }

  /**
   * eip = {{label}}
   */
  def jmp(label: LabelReference): AssemblyLine = {
    new InstructionLine("jmp", Seq(label))
  }

  /**
   * Prepares {{eax}} and {{ebx}} for conditional jumps
   */
  def cmp(eax: Register, ebx: Register): AssemblyLine = {
    new InstructionLine("cmp", Seq(eax, ebx))
  }

  /**
   * {{ == }}
   */
  def je(label: LabelReference): AssemblyLine = {
    new InstructionLine("je", Seq(label))
  }

  /**
   * {{ != }}
   */
  def jne(label: LabelReference): AssemblyLine = {
    new InstructionLine("jne", Seq(label))
  }

  /**
   * Signed {{ > }}
   */
  def jg(label: LabelReference): AssemblyLine = {
    new InstructionLine("jg", Seq(label))
  }

  /**
   * Signed {{ < }}
   */
  def jl(label: LabelReference): AssemblyLine = {
    new InstructionLine("jl", Seq(label))
  }

  /**
   * Signed {{ >= }}
   */
  def jge(label: LabelReference): AssemblyLine = {
    new InstructionLine("jge", Seq(label))
  }

  /**
   * Signed {{ <= }}
   */
  def jle(label: LabelReference): AssemblyLine = {
    new InstructionLine("jle", Seq(label))
  }

  /**
   * Unsigned {{ > }}
   */
  def ja(label: LabelReference): AssemblyLine = {
    new InstructionLine("ja", Seq(label))
  }

  /**
   * Unsigned {{ < }}
   */
  def jb(label: LabelReference): AssemblyLine = {
    new InstructionLine("jb", Seq(label))
  }

  /**
   * Unsigned {{ >= }}
   */
  def jae(label: LabelReference): AssemblyLine = {
    new InstructionLine("jae", Seq(label))
  }

  /**
   * Unsigned {{ <= }}
   */
  def jbe(label: LabelReference): AssemblyLine = {
    new InstructionLine("jbe", Seq(label))
  }

  /**
   * Copies the value of {{eax}} to stack pointer
   * Increments the stack pointer
   */
  def push(eax: Register): AssemblyLine = {
    new InstructionLine("push", Seq(eax))
  }

  /**
   * Copies the value pointed by stack pointer to {{eax}}
   * Decrements the stack pointer
   */
  def pop(eax: Register): AssemblyLine = {
    new InstructionLine("pop", Seq(eax))
  }

  /**
   * Pushes program counter onto the stack
   * Jumps to the label
   */
  def call(label: LabelReference): AssemblyLine = {
    new InstructionLine("call", Seq(label))
  }

  /**
   * Pops the program counter from the stack
   */
  def ret(): AssemblyLine = {
    new InstructionLine("ret", Seq())
  }

  /**
   * System call
   */
  def int(address: Int): AssemblyLine = {
    new InstructionLine("int", Seq(address))
  }

  /**
   * 4-byte data
   */
  def dd(value: Int): AssemblyLine = {
    new InstructionLine("dd", Seq(value))
  }

  def dd(labelReference: LabelReference): AssemblyLine = {
    new InstructionLine("dd", Seq(labelReference))
  }

  /**
   * 2-byte data
   */
  def dw(value: String): AssemblyLine = {
    new InstructionLine("dw", Seq(toExpression(value)))
  }

  /**
   * 1-byte data
   */
  def db(value: String): AssemblyLine = {
    new InstructionLine("db", Seq(toExpression(value)))
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
    new AbstractAssemblyLine {
      override protected def writeContent(writer: PrintWriter) {
        writer.print(line)
      }
    }
  }

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
      override protected def writeContent(writer: PrintWriter) {
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
      override protected def writeContent(writer: PrintWriter) {
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
      override protected def writeContent(writer: PrintWriter) {
        writer.print("extern ")
        label.write(writer)
      }
    }
  }

  /**
   * Defines a label
   */
  def label(name: String): AssemblyLine = {
    new AbstractAssemblyLine {
      override def writeContent(writer: PrintWriter) {
        writer.print(name)
        writer.print(':')
      }
    }
  }

  implicit def toExpression(value: String): AssemblyExpression = {
    new AbstractAssemblyExpression {
      override def write(writer: PrintWriter) {
        writer.print('"')
        writer.print(value)
        writer.print('"')
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
