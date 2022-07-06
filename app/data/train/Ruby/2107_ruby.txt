class UserMailer < ActionMailer::Base
  default :from => "mehmetalitepeli@gmail.com"

  def registration_confirmation(author)
    mail(:to => author.email, :subject => "Registered")
  end
end
