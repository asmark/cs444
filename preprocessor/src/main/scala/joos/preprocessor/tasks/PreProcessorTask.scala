package joos.preprocessor.tasks

import java.io.File
import joos.core.Logger

trait PreProcessorTask {

  protected def isFileExist(path: String): Boolean = isFileExist(new File(path))

  protected def isFileExist(file: File): Boolean = file.isFile && file.exists

  def runTask() {
    dependsOn.foreach(_.runTask())
    if (!isTaskCached()) {
      Logger.logInformation(s"Running ${this.getClass.getName}")
      executeTask()
    } else {
      Logger.logInformation(s"Skipping ${this.getClass.getName}")
    }
  }

  protected def dependsOn: List[PreProcessorTask]

  protected def isTaskCached(): Boolean

  protected def executeTask()

}
