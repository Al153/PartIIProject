name := "PartII"

version := "1.0"

scalaVersion := "2.12.1"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"

// SQL db
libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "3.1.0",
  "com.h2database"  %  "h2"                % "1.4.196",
  "ch.qos.logback"  %  "logback-classic"   % "1.2.3"
)

// Scala test
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)
