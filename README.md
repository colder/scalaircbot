scalaircbot
===========

Simple IRC bot written in scala

## Installation

Run the latest sql script in database/

To run the master branch:
* Create <code>src/main/resources/setup.conf</code> (you can copy <code>src/main/resources/setup.conf.example</code>)
* The <code>setup.conf</code> file may contain multiple sections configuring the bot, such as <code>dev</code> and <code>prod</code>
* <code>sbt "run prod"</code> where "prod" is the configuration you want to run.
* to log to a log file: <code>sbt "run prod" | tee -a bot.log</code>
