#!/bin/sh

git pull origin master
SHA="$(git show -s --format=%H| cut -c1-8)"
echo ${SHA}
sbt compile
sbt "runMain remote.Run" 2>&1 | tee  ~/perfTests/${SHA}.log

