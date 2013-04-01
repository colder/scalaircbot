scalaircbot
===========

Simple IRC bot written in scala

## Installation

Run the latest sql script in database/

To run the master branch:
* first time run: <code>sbt update</code> to grab the dependencies
* Copy config.xml to config-prod.xml
* Edit the file with your parameters
* <code>sbt "run config-prod.xml"</code>
* to log to a log file: <code>sbt "run config-prod.xml" | tee -a bot.log</code>
