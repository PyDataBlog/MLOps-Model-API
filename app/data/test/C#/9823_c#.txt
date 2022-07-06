using Client.Models;
using Server;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Net.Mail;
using System.Web;
using System.Web.Mvc;

namespace Client.Controllers
{
    public class ContactController : BaseController
    {
        [HttpGet]
        public ViewResult Index()
        {
            var model = new ContactModel();
            return View(model);
        }

        [HttpPost, ValidateAntiForgeryToken]
        public ViewResult Index(ContactModel model)
        {

            var contactRequest = new ContactRequest();
            contactRequest.Ad = model.Ad;
            contactRequest.Email = model.Email;
            contactRequest.Telefon = model.Telefon;
            contactRequest.Mesaj = model.Mesaj;
            contactRequest.IP = Request.UserHostName;
            try
            {
                SendMail("info@setcrm.com", "info@setcrm.com", contactRequest.Email, "SetCRM--İletişim Formu ",
                "<stong>Ad :</stong>" + contactRequest.Ad + "  <br/> " +
                "<stong>Email :</stong>" + contactRequest.Email + " <br/> " +
                "<stong>Telefon :</stong>" + contactRequest.Telefon + " <br/> " +
                "<stong>Mesaj :</stong>" + contactRequest.Mesaj + " <br/> " +
                "<stong>IP :</stong>" + contactRequest.IP + " ");
                ViewBag.message = "Mail Başarıyla İletilmiştir.Teşekkürler.";

                model.Ad = "";
                model.Email = "";
                model.Telefon = "";
                model.Mesaj = "";
            }
            catch (Exception)
            {

                ViewBag.message = "Mail Gönderiminde Hata Oluştu Lütfen Daha Sonra Tekrar Deneyiniz.";

            }

            return View(model);
        }
        public static void SendMail(string kimden, string kime, string gonderilenAdSoyad, string konu, string icerik)
        {

            MailMessage message = new MailMessage();

            message.From = new MailAddress(kimden);

            message.To.Add(new MailAddress(kime, gonderilenAdSoyad));

            message.Subject = konu;

            message.Body = icerik;

            message.IsBodyHtml = true;

            System.Net.Mail.SmtpClient smtp = new System.Net.Mail.SmtpClient("smtp.gmail.com");

            smtp.UseDefaultCredentials = false;

            System.Net.NetworkCredential ncredential = new System.Net.NetworkCredential("info@setcrm.com", "");

            smtp.Credentials = ncredential;

            smtp.EnableSsl = true;

            smtp.Port = 587;

            smtp.Send(message);
        }
    }
}