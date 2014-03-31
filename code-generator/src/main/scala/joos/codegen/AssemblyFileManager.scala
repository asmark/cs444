package joos.codegen

import collection.mutable
import java.io.PrintWriter
import joos.assemgen._

class AssemblyFileManager(val fileName: String) {
  val globals = mutable.HashSet.empty[AssemblyLine]
  val data = mutable.MutableList.empty[AssemblyLine]
  val text = mutable.MutableList.empty[AssemblyLine]

  def appendText(lines: AssemblyLine*) {
    text ++= lines
  }

  def appendGlobal(lines: AssemblyLine*) {
    globals ++= lines
  }

  def appendData(lines: AssemblyLine*) {
    data ++= lines
  }
}