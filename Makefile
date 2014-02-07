all:
	sbt clean preprocessor/run compiler/assembly
	mv compiler/target/compiler.jar compiler.jar
