name := "scalaircbot"

version := "1.0"

organization := "colder"

scalaVersion := "2.9.1"

scalacOptions += "-unchecked"

mainClass in (Compile, run) := Some("ircbot.Main")

libraryDependencies ++= Seq(
    "commons-codec" % "commons-codec" % "1.3",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "commons-logging" % "commons-logging" % "1.1.1",
    "mysql" % "mysql-connector-java" % "5.1.15"
)

