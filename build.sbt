name := "PartII"

version := "1.0"

scalaVersion := "2.12.1"


// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"


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

