#!/bin/sh

sbt compile
sbt "runMain remote.Run" > ~/performanceTest.log

