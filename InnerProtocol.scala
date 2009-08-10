package ircbot

/* Inner protocol between Control, Connection and the connection checker */
object InnerProtocol {
    // Request a line from the server
    object ReadLine;
    // Write a line to the server
    case class WriteLine(line: String);
    // Response from the server
    case class ReadLineAnswer(line: Option[String]);
    // Reconnect to the server
    case class ReinitConnection();
}

