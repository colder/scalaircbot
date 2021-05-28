name := "scalaircbot"

version := "2.0"

organization := "colder"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

scalacOptions += "-unchecked"

resolvers += "Typesafe Repository" at "http://repo.typesafe.com/typesafe/releases/"

mainClass in (Compile, run) := Some("ircbot.Main")

libraryDependencies ++= Seq(
    "commons-codec" % "commons-codec" % "1.3",
    "commons-httpclient" % "commons-httpclient" % "3.1",
    "commons-logging" % "commons-logging" % "1.1.1",
    "joda-time" % "joda-time" % "2.9",
    "org.joda" % "joda-convert" % "1.8",
    "com.enragedginger" %% "akka-quartz-scheduler" % "1.4.0-akka-2.3.x",
    "com.typesafe.akka" %% "akka-actor" % "2.4.0",
    "com.typesafe.slick" %% "slick" % "3.1.0",
    "com.typesafe.slick" %% "slick-hikaricp" % "3.1.0",
    "com.typesafe" % "config" % "1.3.0",
    "mysql" % "mysql-connector-java" % "8.0.23",
    "org.slf4j" % "slf4j-nop" % "1.6.4"
)

