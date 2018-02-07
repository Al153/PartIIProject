#!/bin/sh

git pull origin master
sbt compile
sbt "runMain remote.Run" 2>&1 | tee  ~/performanceTest.log

