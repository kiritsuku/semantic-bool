name := "semantic-bool"
     
version := "0.1"
     
scalaVersion := "2.10.0-RC2"

scalacOptions ++= Seq("-deprecation", "-feature", "-Xlint")

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10.0-RC2" % "2.0.M5" % "test",
  "junit" % "junit" % "4.10" % "test"
)

EclipseKeys.withSource := true