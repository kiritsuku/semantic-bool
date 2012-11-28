name := "semantic-bool"
     
version := "0.1"
     
scalaVersion := "2.10.0-RC2"

scalacOptions ++= Seq(
  "-deprecation",
  "-feature",
  "-unchecked",
  "-Xlint",
  "-Xlog-reflective-calls",
  "-Ywarn-adapted-args",
  "-encoding", "UTF-8",
  "-target:jvm-1.6"
)

libraryDependencies ++= Seq(
  "org.scalaz" % "scalaz-core_2.10.0-RC2" % "7.0.0-M4"
)

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest_2.10.0-RC2" % "2.0.M5" % "test",
  "junit" % "junit" % "4.10" % "test",
  "org.scalacheck" % "scalacheck_2.10.0-RC2" % "1.10.0" % "test"
)

EclipseKeys.withSource := true
