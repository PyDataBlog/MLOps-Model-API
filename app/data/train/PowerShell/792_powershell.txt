# this script removes /esss-website/ from all links and replaces it with beta.esss.ca

$phraseToRemove = "/esss-website"
$phraseToInsert = "http://beta.esss.ca"
$filepath = "C:\Users\Shayne\Documents\GitHub\esss-website"

$files = get-childitem $filepath -include *.html, *.js, *.css -recurse
foreach($file in $files) {
	((get-content $file.fullname) -creplace $phraseToRemove, $phraseToInsert) | 
	set-content $file.fullname
}