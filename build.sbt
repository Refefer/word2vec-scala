name := "word2vec"

version := "1.0.0-SNAPSHOT"

scalaVersion := "2.10.3"

val spireVersion = "0.8.2"

libraryDependencies ++= Seq(
  // spire
  "org.spire-math" % "spire_2.10" % spireVersion 
)

javaOptions += "-Xmx4G"

fork := true
