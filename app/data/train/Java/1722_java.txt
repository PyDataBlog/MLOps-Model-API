package com.hcentive.webservice.soap;


import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.ws.server.endpoint.annotation.Endpoint;
import org.springframework.ws.server.endpoint.annotation.PayloadRoot;
import org.springframework.ws.server.endpoint.annotation.RequestPayload;
import org.springframework.ws.server.endpoint.annotation.ResponsePayload;

import com.hcentive.service.FormResponse;
import com.hcentive.webservice.exception.HcentiveSOAPException;

import org.apache.log4j.Logger;
/**
 * @author Mebin.Jacob
 *Endpoint class.
 */
@Endpoint
public final class FormEndpoint {
	private static final String NAMESPACE_URI = "http://hcentive.com/service";
	Logger logger = Logger.getLogger(FormEndpoint.class); 

	@Autowired
	FormRepository formRepo;
	
	@PayloadRoot(namespace = NAMESPACE_URI, localPart = "FormResponse")
	@ResponsePayload
	public FormResponse submitForm(@RequestPayload FormResponse request) throws HcentiveSOAPException {
//		GetCountryResponse response = new GetCountryResponse();
//		response.setCountry(countryRepository.findCountry(request.getName()));
		FormResponse response = null;
		logger.debug("AAGAYA");
		try{
			response = new FormResponse();
			response.setForm1(formRepo.findForm("1"));
			//make API call
		}catch(Exception exception){
			throw new HcentiveSOAPException("Something went wrong!!! The exception is --- " + exception);
		}
		
		return response;
//		return null;
	}
	
}
