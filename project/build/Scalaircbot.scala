import sbt._

class ScalaIRCBotProject(info: ProjectInfo) extends DefaultProject(info) {
	val codec = "commons-codec" % "commons-codec" % "1.3"
	val httpclient = "commons-httpclient" % "commons-httpclient" % "3.1"
	val logging = "commons-logging" % "commons-logging" % "1.1.1"
	val mysql = "mysql" % "mysql-connector-java" % "5.1.15"
}
