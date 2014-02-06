package joos.preprocessor.tasks

import java.io.File

trait PreProcessorTask {

  protected def isFileExist(path: String): Boolean = isFileExist(new File(path))

  protected def isFileExist(file: File): Boolean = file.isFile && file.exists

  def runTask() {
    dependsOn.foreach(_.runTask())
    if (!isTaskCached()) {
      executeTask()
    }
  }

  protected def dependsOn: List[PreProcessorTask]

  protected def isTaskCached(): Boolean

  protected def executeTask()

}
