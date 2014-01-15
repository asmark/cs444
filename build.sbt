// Project properties
name := "Joos1W Compiler"

version := "1.0"

scalaVersion := "2.10.3"

// Dependencies
libraryDependencies += "com.eed3si9n" %% "treehugger" % "0.3.0"

resolvers += Resolver.sonatypeRepo("public")

// IntelliJ config
ideaExcludeFolders += ".idea"

ideaExcludeFolders += ".idea_modules"
