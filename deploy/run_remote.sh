#!/bin/sh

git pull origin master
SHA="$(git show -s --format=%H| cut -c1-8)"
echo ${SHA}
sbt compile
sbt test 2>&1 | tee ~/perfTests/lastTests.log
export SBT_OPTS="-Xmx1536M -XX:+UseConcMarkSweepGC -XX:+CMSClassUnloadingEnabled -XX:MaxPermSize=2G -Xss2M  -Duser.timezone=GMT"
sbt "runMain remote.Run" 2>&1 | tee  ~/perfTests/${SHA}.log | tee ~/perfTests/last.log


