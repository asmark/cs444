package joos.preprocessor.tasks

import scala.io.Source

trait PreProcessorTask {

  final val ResourceDir = "/"

  final def getResource(file: String): Source = {
    Source.fromURL(getClass.getResource(file))
  }

  final def isFileExist(file: String): Boolean = {
    Source.fromURL(getClass.getResource(ResourceDir)).getLines().contains(file)
  }

  final def getResourceDirPath(): String = {
    getClass.getResource(ResourceDir).getPath
  }

  final def runTask() {
    dependsOn.foreach(_.runTask())
    if (!isTaskCached()) {
      executeTask()
    }
  }

  protected def dependsOn: List[PreProcessorTask]

  protected def isTaskCached(): Boolean

  protected def executeTask()

}
