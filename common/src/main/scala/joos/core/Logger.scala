package joos.core

object Logger {
  private[this] val LogOff = Int.MaxValue
  private[this] val LogError = 3
  private[this] val LogWarning = 2
  private[this] val LogInformation = 1
  private[this] val LogLevel = LogInformation


  def logError(text: String): this.type = {
    if (LogLevel <= LogError) {
      Console.err.println(Console.RED + "ERROR: " + text + Console.RESET)
    }
    this
  }

  def logWarning(text: String): this.type = {
    if (LogLevel <= LogWarning) {
      Console.err.println(Console.YELLOW + "WARN: " + text + Console.RESET)
    }
    this
  }

  def logInformation(text: String): this.type = {
    if (LogLevel <= LogInformation) {
      Console.err.println(Console.BLUE + "INFO: " + text + Console.RESET)
    }
    this
  }
}
