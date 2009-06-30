all: scalafiles

scalafiles:
	fsc -unchecked -deprecation -classpath libs/mysql-connector-java-3.0.17-ga-bin.jar -d classes `find . -name "*.scala"`

test:
	scala -cp classes:libs/mysql-connector-java-3.0.17-ga-bin.jar ircbot.Main config-test.xml

run:
	scala -cp classes:libs/mysql-connector-java-3.0.17-ga-bin.jar ircbot.Main config-prod.xml
