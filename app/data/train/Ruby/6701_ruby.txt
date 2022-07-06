#api key - 567zg2xF9MGrCFwPllPkTg

require 'mandrill' # hey i need this gem
client = Mandrill::API.new '567zg2xF9MGrCFwPllPkTg'

email = {
  "preserve_recipients" => nil,
  "text" => "Hello from your secret admirerer. I love you :)",
  "to" => [{"type" => "to", "email" => "paul.busby@gmail.com", "name" => "my love"}],
  "from_name" => "From your secret admirerer ",
  "from_email" => "kate.m.shirley@gmail.com",
  "subject" => "I LOVE YOU",
  "return_path_domain" => nil
  }

  result = client.messages.send email

  puts 'hey an email should be sent soon'
  puts result
