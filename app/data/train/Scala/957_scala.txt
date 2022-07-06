package edu.lu.uni.serval.idempotent.comm

import com.google.gson.{Gson, JsonObject}
import com.rabbitmq.client.AMQP.BasicProperties
import com.rabbitmq.client._

/**
  * Created by darkrsw on 2016/November/22.
  */
object ResultSender
{
  var host = "localhost"
  var QUEUE_NAME = "test"

  var ALARM_QUEUE_NAME = "js-alarms"
  var LOG_QUEUE_NAME = "js-logs"
  var PLOG_QUEUE_NAME = "js-prj-logs"
  var CLOG_QUEUE_NAME = "js-commit-logs"

  var TASK_QUEUE_NAME = "js-task-q"

  var RABBITMQ_USER = ""
  var RABBITMQ_PASSWD = ""

  val gson = new Gson()

  var conn: Connection = _
  var channel: Channel = _

  def init(): Unit =
  {
    val factory = new ConnectionFactory()
    factory.setHost(host)
    factory.setUsername(RABBITMQ_USER)
    factory.setPassword(RABBITMQ_PASSWD)

    conn = factory.newConnection()
    channel = conn.createChannel()
  }

  def getOneTask(): String =
  {
    val response = channel.basicGet(TASK_QUEUE_NAME, true)

    if( response == null )
      return null


    val msg = new String(response.getBody, "UTF-8")

    return msg
  }


  def sendResult(queue: String, msg: String) =
  {
    channel.synchronized {
      channel.basicPublish("", queue, null, msg.getBytes())
    }
    Console.println("To [%s] Sent '".format(queue) + msg + "'")
  }

  def sendCommitLog(project: String, commit: String, result: String, errmsg: String) =
  {
    val root = new JsonObject()
    root.addProperty("project", project)
    root.addProperty("commit", commit)
    root.addProperty("result", result)
    root.addProperty("errmsg", errmsg)

    sendResult(CLOG_QUEUE_NAME, gson.toJson(root))
  }

  def sendProjectLog(project: String, result: String, errmsg: String) =
  {
    val root = new JsonObject()
    root.addProperty("project", project)
    root.addProperty("result", result)
    root.addProperty("errmsg", errmsg)

    sendResult(PLOG_QUEUE_NAME, gson.toJson(root))
  }

  def sendLog(project: String, filepath: String, commit: String, result: String, errmsg: String) =
  {
    val root = new JsonObject()
    root.addProperty("project", project)
    root.addProperty("commit", commit)
    root.addProperty("filepath", filepath)
    root.addProperty("result", result)
    root.addProperty("errmsg", errmsg)

    sendResult(LOG_QUEUE_NAME, gson.toJson(root))
  }

  def sendAlarm(project: String, commit: String, alarm: String) =
  {
    val root = new JsonObject()
    root.addProperty("project", project)
    root.addProperty("commit", commit)
    root.addProperty("alarm", alarm)

    sendResult(ALARM_QUEUE_NAME, gson.toJson(root))
  }

  def close(): Unit =
  {
    channel.close()
    conn.close()
  }

  def main(args: Array[String]): Unit =
  {
    // TODO: just for testing.
    val alarm =
      """"/content/cn/steedos/buy.html","12","139:5038","139:5086","6","Conditional expression 'listprice.trim() == '' || listprice_rmb.trim() == ''' is always true."""".stripMargin

    val gson = new Gson()

    val root = new JsonObject()
    root.addProperty("project", "steedos$steedos")
    root.addProperty("commit", "ba40bb8429a81ecae3aae4e5acabaa90b122b2a8")
    root.addProperty("alarm", alarm)


    sendResult("js-alarms", gson.toJson(root))


    val logroot = new JsonObject()
    logroot.addProperty("project", "steedos$steedos")
    logroot.addProperty("commit", "ba40bb8429a81ecae3aae4e5acabaa90b122b2a8")
    logroot.addProperty("log", "SUCCESS=>23748")

    sendResult("js-logs", gson.toJson(logroot))


    close()
    Runtime.getRuntime.exit(0)
  }
}
