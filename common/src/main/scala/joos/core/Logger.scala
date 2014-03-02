package joos.core

object Logger {
  def logError(text: String): this.type = {
    Console.err.print(Console.RED)
    Console.err.println(text)
    Console.err.print(Console.RESET)
    this
  }

  def logWarning(text: String): this.type = {
    Console.err.print(Console.YELLOW)
    Console.err.println(text)
    Console.err.print(Console.RESET)
    this
  }

  def logInformation(text: String): this.type = {
    Console.err.print(Console.GREEN)
    Console.err.println(text)
    Console.err.print(Console.RESET)
    this
  }
}
