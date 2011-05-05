package ircbot

/* Inner protocol between Control, Connection and the connection checker */
object InnerProtocol {
    // Request a line from the server
    object ReadLine;

    object StartListening
    object StopListening

    // Write a line to the server
    case class WriteLine(line: String);
    // Response from the server
    case class ReadLineAnswer(line: Option[String]);
    // Connect to the server
    case object InitConnection;
    // Close the connection to the server
    case object CloseConnection;
    // Reinit the connection to the server
    case object ReinitConnection;
    // Reconnect to the server
    case object StopChecker;
}

