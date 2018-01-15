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

// LMBD
scalacOptions += "-feature"

libraryDependencies += "org.deephacks.lmdbjni" % "lmdbjni" % "0.4.6"
libraryDependencies += "org.deephacks.lmdbjni" % "lmdbjni-win64" % "0.4.6"
// libraryDependencies += "org.deephacks.lmdbjni" % "lmdbjni-osx64" % "0.4.6"
// libraryDependencies += "org.deephacks.lmdbjni" % "lmdbjni-linux64" % "0.4.6"

// for building the imdb database
libraryDependencies += "io.spray" %%  "spray-json" % "1.3.3"