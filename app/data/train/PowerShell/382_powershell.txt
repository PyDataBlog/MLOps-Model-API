[System.Net.ServicePointManager]::DnsRefreshTimeout = 0

#change these variables
$url = "web.com"
$output = "$PSScriptRoot\temperature.json"

#credit for this code goes to Aaron from http://sharpcodenotes.blogspot.com/2013/03/how-to-make-http-request-with-powershell.html
function Format-XML {
  Param ([string]$xml)
 
  $out = New-Object System.IO.StringWriter
  $Doc=New-Object system.xml.xmlDataDocument 
  $doc.LoadXml($xml) 
  $writer=New-Object system.xml.xmltextwriter($out) 
  $writer.Formatting = [System.xml.formatting]::Indented 
  $doc.WriteContentTo($writer) 
  $writer.Flush()
  $out.flush()
  Write-Output $out.ToString()
}
function Http-Web-Request([string]$method,[string]$encoding,[string]$server,[string]$path,$headers,[string]$postData)
{
    ## Compose the URL and create the request
    $url = "$server/$path"
    [System.Net.HttpWebRequest] $request = [System.Net.HttpWebRequest] [System.Net.WebRequest]::Create($url)
 
    ## Add the method (GET, POST, etc.)
    $request.Method = $method
    ## Add an headers to the request
    foreach($key in $headers.keys)
    {
        $request.Headers.Add($key, $headers[$key])
    }
    ## We are using $encoding for the request as well as the expected response
    $request.Accept = $encoding
    ## Send a custom user agent if you want
    $request.UserAgent = "PowerShell script"
 
    ## Create the request body if the verb accepts it (NOTE: utf-8 is assumed here)
    if ($method -eq "POST" -or $method -eq "PUT") {
        $bytes = [System.Text.Encoding]::UTF8.GetBytes($postData) 
        $request.ContentType = $encoding
        $request.ContentLength = $bytes.Length
         
        [System.IO.Stream] $outputStream = [System.IO.Stream]$request.GetRequestStream()
        $outputStream.Write($bytes,0,$bytes.Length)  
        $outputStream.Close()
    }
 
    ## This is where we actually make the call.  
    try
    {
        [System.Net.HttpWebResponse] $response = [System.Net.HttpWebResponse] $request.GetResponse()     
        $sr = New-Object System.IO.StreamReader($response.GetResponseStream())       
        $txt = $sr.ReadToEnd() 
        ## NOTE: comment out the next line if you don't want this function to print to the terminal
        #Write-Host "CONTENT-TYPE: " $response.ContentType
        ## NOTE: comment out the next line if you don't want this function to print to the terminal
        #Write-Host "RAW RESPONSE DATA:" . $txt
        ## If we have XML content, print out a pretty version of it
        if ($response.ContentType.StartsWith("text/xml"))
        {
            ## NOTE: comment out the next line if you don't want this function to print to the terminal
            Format-XML($txt)
        }
        ## Return the response body to the caller
        return $txt
    }
    ## This catches errors from the server (404, 500, 501, etc.)
    catch [Net.WebException] { 
        [System.Net.HttpWebResponse] $resp = [System.Net.HttpWebResponse] $_.Exception.Response  
        ## NOTE: comment out the next line if you don't want this function to print to the terminal
        Write-Host $resp.StatusCode -ForegroundColor Red -BackgroundColor Yellow
        ## NOTE: comment out the next line if you don't want this function to print to the terminal
        Write-Host $resp.StatusDescription -ForegroundColor Red -BackgroundColor Yellow
        ## Return the error to the caller
        return $resp.StatusDescription
    }
}
$headers = @{ 
    apikey = "your authentication key"
}

#Downloads website
$result = Http-Web-Request "GET" $output $url "" $headers ""
#Extracts website into output file
$result > $output
