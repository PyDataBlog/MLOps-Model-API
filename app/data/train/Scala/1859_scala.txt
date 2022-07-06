package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._
import models._
import play.api.Play.current
import com.typesafe.plugin._

object Application extends Controller {

  val accountForm = Form(
    mapping(
      "name" -> nonEmptyText,
      "email" -> email
    )(Account.apply)(Account.unapply)
  )

  def index = Action { implicit request =>
    Ok(views.html.index(accountForm.fill(Account("", ""))))
  }

  def submit = Action { implicit request =>
    accountForm.bindFromRequest.fold(
      errors => BadRequest(views.html.index(accountForm = errors)),
      account => {
        val mail = use[MailerPlugin].email
        mail.setSubject("Registered account")
        mail.setRecipient(s"${account.name} <${account.mailAddress}>")
        mail.setFrom("Playframework <noreply@example.com>")
        mail.send(
          views.txt.mail.registeredAccount(account).body.trim,
          views.html.mail.registeredAccount(account).body.trim
        )

        Redirect(routes.Application.index).flashing(
          "success" -> "Sent email."
        )
      }
    )
  }

}
