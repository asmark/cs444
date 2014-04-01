package joos.codegen

import collection.mutable
import joos.assemgen._
import joos.ast.expressions.SimpleNameExpression

class AssemblyFileManager(val fileName: String) {
  val globals = mutable.HashSet.empty[String]
  val data = mutable.MutableList.empty[AssemblyLine]
  val text = mutable.MutableList.empty[AssemblyLine]

  def appendText(lines: AssemblyLine*) {
    text ++= lines
  }

  def appendGlobal(lines: String*) {
    globals ++= lines
  }

  def appendData(lines: AssemblyLine*) {
    data ++= lines
  }
}
