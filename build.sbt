seq(assemblySettings : _*)

name := "scalaircbot"

version := "1.0"

organization := "colder"

scalaVersion := "2.10.3"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

mainClass in (Compile, run) := Some("ircbot.Main")

libraryDependencies ++= Seq(
    "commons-codec" % "commons-codec" % "1.3",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "commons-logging" % "commons-logging" % "1.1.1",
    "mysql" % "mysql-connector-java" % "5.1.15",
    "com.typesafe.akka" %% "akka-actor" % "2.2.3",
    "com.typesafe.slick" %% "slick" % "2.0.0",
    "org.slf4j" % "slf4j-nop" % "1.6.4"
)

