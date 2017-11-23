name := "PartII"

version := "1.0"

scalaVersion := "2.12.1"


// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"

// SQL db
libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc"       % "3.1.0",
  "com.h2database"  %  "h2"                % "1.4.196",
  "ch.qos.logback"  %  "logback-classic"   % "1.2.3"
)

// Doobie db
libraryDependencies ++= Seq(
  "org.tpolecat" %% "doobie-core"      % "0.4.4", // scalaz + scalaz-stream
  "org.tpolecat" %% "doobie-postgres"      % "0.4.4"
)

// Scala test
libraryDependencies ++= Seq(
  "org.scalactic" %% "scalactic" % "3.0.1",
  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
)

// Junit
libraryDependencies ++= Seq(
  "junit" % "junit" % "4.11" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
    exclude("junit", "junit-dep")
)

