name := "pomdp-solver"

version := "0.5"

scalaVersion := "2.12.12"

val sparkVersion = "2.2.0"

libraryDependencies ++= Seq(
  // "org.apache.spark" %% "spark-core" % sparkVersion % "provided",
  // "org.apache.spark" %% "spark-sql" % sparkVersion % "provided",
  // "org.apache.spark" %% "spark-mllib" % sparkVersion % "provided",
  "io.spray" %% "spray-json" % "1.3.3",
  "com.typesafe" % "config" % "1.2.1"
)

resolvers += Classpaths.typesafeReleases

mainClass in(Compile, run) := Some("POMDPMain")
mainClass in(Compile, packageBin) := Some("POMDPMain")

