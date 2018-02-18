name := "PartII"

version := "1.0"

scalaVersion := "2.12.1"


// Scalaz
libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.2.15"

// Escape strings for SQL
libraryDependencies += "commons-lang" % "commons-lang" % "2.5"

// postgresql driver
libraryDependencies += "org.postgresql" % "postgresql" % "42.1.4"

// Scala test
//libraryDependencies ++= Seq(
//  "org.scalactic" %% "scalactic" % "3.0.1",
//  "org.scalatest" %% "scalatest" % "3.0.1" % "test"
//)

// Junit
libraryDependencies ++= Seq(
  "junit" % "junit" % "4.11" % Test,
  "com.novocode" % "junit-interface" % "0.11" % Test
    exclude("junit", "junit-dep")
)

// LMBD
scalacOptions += "-feature"
libraryDependencies += "org.lmdbjava" %	"lmdbjava"% "0.6.0"

// for building the imdb database
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"

// Logging
libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5"
)