#!/bin/bash
mvn package
java -jar target/botapi-7.2.0-jar-with-dependencies.jar
