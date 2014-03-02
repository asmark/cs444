package joos.core

object Logger {
  private[this] val LogOff = Int.MaxValue
  private[this] val LogError = 3
  private[this] val LogWarning = 2
  private[this] val LogInformation = 1
  private[this] val LogLevel = LogError


  def logError(text: String): this.type = {
    if (LogLevel <= LogError) {
      Console.err.print(Console.RED)
      Console.err.println(text)
      Console.err.print(Console.RESET)
    }
    this
  }

  def logWarning(text: String): this.type = {
    if (LogLevel <= LogWarning) {
      Console.err.print(Console.YELLOW)
      Console.err.println(text)
      Console.err.print(Console.RESET)
    }
    this
  }

  def logInformation(text: String): this.type = {
    if (LogLevel <= LogInformation) {
      Console.err.print(Console.BLUE)
      Console.err.println(text)
      Console.err.print(Console.RESET)
    }
    this
  }
}
