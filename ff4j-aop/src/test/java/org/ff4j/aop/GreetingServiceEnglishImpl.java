package org.ff4j.aop;

import org.springframework.stereotype.Component;

@Component("greeting.english")
public class GreetingServiceEnglishImpl implements GreetingService {
	
	public String sayHello(String name) {
		return "Hello " + name;
	}

	@Override
	public String sayHelloWithClass(String name) {
		return "Hi " + name;
	}

}