package ircbot.sql

import java.sql.SQLException
import java.sql.PreparedStatement
import java.sql.ResultSet
import java.util.Date

abstract class SQLConnection {
  var conn: Option[java.sql.Connection];
  def connect;

  def prepareStatement(sql: String): SQLStatement = conn match {
    case Some(x) => new SQLStatement(x.prepareStatement(sql))
    case None => throw new Exception("No connection")
  }


  def lastInsertID: Int = {
    prepareStatement("SELECT LAST_INSERT_ID()").executeQuery.firstRow.getInt(1)
  }

  def prepareStatement(sql: String, args:Any*): SQLStatement = {
    checkConnection

    val stmt = conn match {
      case Some(x) => x.prepareStatement(sql)
      case None => throw new Exception("No connection")
    }

    var index = 1

    for(arg <- args) {
      arg match {
        case op: Option[_] =>
          if (op.isEmpty) {
            stmt.setNull(index, java.sql.Types.NULL)
          } else {
            op.get match {
            case ad: Date =>
              stmt.setTimestamp(index, new java.sql.Timestamp(ad.getTime()))
            case as: String =>
              stmt.setString(index, as)
            case ai: Int =>
              stmt.setInt(index, ai)
            case _ =>
              throw new Exception("Invalid type of argument passed")
            }
          }
          index += 1
        case ad: Date =>
          stmt.setTimestamp(index, new java.sql.Timestamp(ad.getTime()))
          index += 1
        case as: String =>
          stmt.setString(index, as)
          index += 1
        case ai: Int =>
          stmt.setInt(index, ai)
          index += 1
        case aai: List[_] =>
          for (a <- aai) {
            a match {
              case i: Int => 
                stmt.setInt(index, i)
              case s: String =>
                stmt.setString(index, s)
            }
            index += 1
          }
        case _ =>
          throw new Exception("Invalid type of argument passed")
      }
    }

    new SQLStatement(stmt)
  }

  def close = conn match {
    case Some(x) => x.close; conn = None
    case None => throw new Exception("No connection");
  }

  def handleException(ex: Exception) = ex match {
    case s:SQLException =>
      println("SQLException: " + s.getMessage +" ("+s.getErrorCode+")")
      s.printStackTrace

    case e: Exception =>
      println("Exception: " + e.getMessage)
      e.printStackTrace
  }

  def checkConnection = conn match {
    case Some(c) =>
      try
      {
        c.createStatement().close();
      } catch  {
        case ex: Exception =>
        connect
      }
    case _ =>
  }
}

class SQLStatement(stmt: PreparedStatement) {
  def executeQuery = new SQLResultSet(stmt.executeQuery)
  def executeUpdate = stmt.executeUpdate
  def close = stmt.close
}

class SQLResultSet(set: ResultSet) {
  def foreach(f: ResultSet => Unit): Unit = {
    while(hasNext) f(set)
  }

  def hasNext = set.next

  def firstRow: ResultSet = { set.beforeFirst; set.next; set }
}
