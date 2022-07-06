# fsm-fondle:
# Touch/force a connection to a website.
# We are not sending proper request, instead just sending traffic against
# the website so we can detect it was "touched" by our script's noodly
# appendages... just like the flying spaghetti monster.
# 
# AUTHOR
# Mauricio Tavares <raubvogel@gmail.com>
#
# RUNNING IT
# 1. Run it once:
# powershell -ExecutionPolicy ByPass -File .\fsm-fondle.ps1
#
# 2. To run it as a recurring job, you can use schtasks:
# Schtasks /create /tn "Fondle site" /sc daily /st 07:00 /tr "powershell -ExecutionPolicy ByPass -File \full\path\to\fsm-fondle.ps1"
#
# VERSIONS
#  v1.0.0. (20141107) : Initial release
#  v1.1.0. (20141113) : $source can now handle multiple arguments separated
#                       by spaces
#

# Edit this as needed
$source = "http://www.website1.com http://www.website2.com:8080/index.jsp?argName=heyThere"

function FondleWebsite
{
   foreach ( $i in $args )
   {
      try
      {
         $request = [System.Net.WebRequest]::Create($i)
         $reply = $request.GetResponse()
      }
      catch
      {
         # I tried really hard to care about error messages but I failed
      }
   }
}

FondleWebsite $source

