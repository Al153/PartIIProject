#!/bin/sh

sbt compile
sbt "runMain remote.Run" 2>&1 | tee  ~/performanceTest.log

