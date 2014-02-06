package joos

import joos.a1.ScanParseWeed

object Compiler {
  def main(args: Array[String]) {
    println(args(1))
    sys.exit(ScanParseWeed(args(1)))
  }
}
