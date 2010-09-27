libs = libs/mysql-connector-java-3.0.17-ga-bin.jar:libs/commons-httpclient-3.1.jar:libs/commons-logging-1.1.1.jar:libs/commons-codec-1.3.jar

all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -classpath ${libs} -d classes `find . -name "*.scala"`

test: scalafiles
	scala -cp classes:${libs} ircbot.Main config-test.xml

run: scalafiles onlyrun

onlyrun:
	scala -cp classes:${libs} ircbot.Main config-prod.xml 2>&1 | tee -a bot.log
