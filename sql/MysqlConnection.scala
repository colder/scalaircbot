package ircbot.sql

import java.sql.DriverManager
import java.sql.Connection
import java.sql.SQLException

class MysqlConnection(host: String, port: Int, database: String, username: String, password: String) extends SQLConnection {
    var conn: Option[Connection] = None


    def connect = {
        try {
            Class.forName("com.mysql.jdbc.Driver");
            conn = Some(DriverManager.getConnection("jdbc:mysql://" + host + ":" + port + "/"+database, username, password));
        } catch {
            case ex: SQLException =>
                handleException(ex)
        }
    }

    connect
}
