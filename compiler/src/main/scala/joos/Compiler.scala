package joos

import joos.a1.ScanParseWeed

object Compiler {
  def main(args: Array[String]) {
    sys.exit(ScanParseWeed(args(0)))
  }
}
