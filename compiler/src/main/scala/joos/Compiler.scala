package joos

import joos.a1.SyntaxCheck

object Compiler {
  def main(args: Array[String]) {
    SyntaxCheck(args(0)) match {
      case Some(parseTree) => sys.exit(0)
      case None => sys.exit(42)
    }
  }
}
