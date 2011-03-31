import sbt._
import Process._

class ScalaIRCBotProject(info: ProjectInfo) extends DefaultProject(info) {
	override def mainClass: Option[String] = Some("ircbot.Main")
	lazy val go = task { 
		"scala -cp target/scala_2.8.1/classes:lib_managed/scala_2.8.1/compile/* ircbot.Main config-prod.xml 2>&1 | tee -a bot.log" !;
	 	None 
	} dependsOn(compile)

	val codec = "commons-codec" % "commons-codec" % "1.3"
	val httpclient = "commons-httpclient" % "commons-httpclient" % "3.1"
	val logging = "commons-logging" % "commons-logging" % "1.1.1"
	val mysql = "mysql" % "mysql-connector-java" % "5.1.15"
}
