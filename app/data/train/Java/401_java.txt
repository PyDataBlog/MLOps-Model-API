package simple.practice;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;

import simple.practice.service.GreetingService;

@Component
public class Greeting {

	@Autowired
	private GreetingService greetingService;
	
	public String printGreeting(String name){
		String msg = greetingService.greetingMessage() + " User: " + name;
		System.out.println(msg);
		return msg;
	}
}
