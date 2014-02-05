package joos.preprocessor.tasks

import java.nio.file.{Paths, Files}
import java.util.Properties
import java.io.File

trait PreProcessorTask {

  final val buildProperties = new Properties()
  final val propertyResource = getClass.getResourceAsStream("/build.properties")
  buildProperties.load(propertyResource)
  propertyResource.close()

  final def getProperty(property: String) = {
    buildProperties.getProperty(property)
  }

  final lazy val managedResourceDir = getProperty("managed-resource-directory")
  final val resourceDir = getClass.getResource("/").getPath

  final def isFileExist(path: String): Boolean = {
    val file = new File(path)
    return file.isFile && file.exists
  }

  final def getResourceDirPath(): String = {
    managedResourceDir
  }

  final def getPathToManagedResource(file: String): String = {
    getResourceDirPath() + "/" + file
  }

  final def getPathToResource(file: String): String = {
    resourceDir + "/" + file
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
