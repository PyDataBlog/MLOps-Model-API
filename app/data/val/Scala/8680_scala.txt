package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {
  // Time to sleep in minutes
  val sleepTime = 60
  
  def index(path: String) = Action {
    Logger.info("Received request for [%s], sleeping for [%d] minutes".format(path, this.sleepTime))

    Thread.sleep(sleepTime * 1000 * 60)
    Ok(views.html.index("Your new application is ready."))
  }
  
}