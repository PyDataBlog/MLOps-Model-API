
package com.service

import javax.jws.WebService
import javax.jws.WebMethod

@WebService(name = "HelloService",
        targetNamespace = "http://jws.samples.geronimo.apache.org")
class HelloService {

  @WebMethod
  def divide(name: String): String = {
    "Hello " + name
  }

}