#!/bin/sh

git pull origin master
SHA="$(git show -s --format=%H| cut -c1-8)"
tmpDir="/local/scratch-2/at736"


echo ${SHA} > ~/${tmpDir}/lastSha.log
sbt compile
sbt test 2>&1 | tee ~/${tmpDir}/lastTests.log
sbt "runMain remote.Run" 2>&1 | tee  ~/${tmpDir}/${SHA}.log | tee ~/perfTests/last.log


