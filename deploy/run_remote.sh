#!/bin/sh

git pull origin master
SHA="$(git show -s --format=%H| cut -c1-8)"

echo ${SHA} > ~/perfTests/lastSha.log
sbt compile
sbt test 2>&1 | tee ~/perfTests/lastTests.log
sbt "runMain remote.Run" 2>&1 | tee  ~/perfTests/${SHA}.log | tee ~/perfTests/last.log


