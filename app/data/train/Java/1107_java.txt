/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package services;

import FareCalculator.Calculate;
import java.util.ArrayList;
import javax.ws.rs.core.Context;
import javax.ws.rs.core.UriInfo;
import javax.ws.rs.PathParam;
import javax.ws.rs.Consumes;
import javax.ws.rs.DELETE;
import javax.ws.rs.PUT;
import javax.ws.rs.Path;
import javax.ws.rs.GET;
import javax.ws.rs.POST;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.lang.ClassNotFoundException;
import java.net.URI;
import javax.ws.rs.DefaultValue;
import javax.ws.rs.QueryParam;
import javax.xml.bind.JAXBElement;
import javax.xml.namespace.QName;
import localstorage.FaresType;

/**
 *
 * @author peppa
 */
@Path("/")

public class calculatorService {

    @Context
    private UriInfo context;
   
    public calculatorService(){
        
    }
    
    
   @GET 
   @Path("/calculate")
   @Produces({"application/xml"})
   public Response fareCalculator(@DefaultValue("TK") @QueryParam("carrier") String carrier,
                                  @DefaultValue("2012-01-01") @QueryParam("date") String date,
                                  @DefaultValue("ADB") @QueryParam("origCode") String origCode,
                                  @DefaultValue("ESB") @QueryParam("destCode") String destCode,
                                  @DefaultValue("Economy") @QueryParam("fareClass") String fareClass) {

        Calculate cal = new Calculate();
        FaresType fare = cal.fareCalculate(carrier, date, origCode, destCode, fareClass);
        
        return Response.ok(new JAXBElement<FaresType>(new QName("faresType"), FaresType.class, fare)).build();

        
   }
    
   
}
