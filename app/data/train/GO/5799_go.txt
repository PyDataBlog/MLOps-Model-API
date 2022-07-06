package crossdomain

import (
  "fmt"
  "net/http"
  )
  
// make a struct for our http handler
type CDHandler struct {}

// use to build the xml
type crossdomain struct {
}

func (*c CDHandler) serveHTTP(w http.ResponseWriter, r *http.Request) {

  // serve the relevant crossdomain.xml stuff here
  // simple to begin
  fmt.Fprint(w, "<?xml version="1.0" ?> \n <cross-domain-policy> \n <allow-access-from domain="*" /> \n </cross-domain-policy>"
)
}
