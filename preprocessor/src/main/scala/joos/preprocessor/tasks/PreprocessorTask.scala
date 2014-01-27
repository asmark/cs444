package joos.preprocessor.tasks

import scala.io.Source

trait PreprocessorTask {

  def dependsOn: List[PreprocessorTask]

  def isTaskCached(): Boolean

  def executeTask()

  def getResource(file: String): Source = {
    Source.fromURL(getClass.getResource(file))
  }

  def getResourceDir(): String = {
    getClass.getResource("/").getPath
  }

}
