all:
	sbt clean preprocessor/run compiler/assembly
	mv compiler/target/compiler.jar compiler.jar
zip:
	sbt clean
	rm cs-444.zip
	zip -r -q cs-444 common scanner preprocessor parser project compiler Makefile joosc
