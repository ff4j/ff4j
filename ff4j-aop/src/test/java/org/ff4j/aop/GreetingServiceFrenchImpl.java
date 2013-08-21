package org.ff4j.aop;

import org.springframework.stereotype.Component;

@Component("greeting.french")
public class GreetingServiceFrenchImpl implements GreetingService {
	
	public String sayHello(String name) {
		return "Bonjour " + name;
	}

	@Override
	public String sayHelloWithClass(String name) {
		return "Salut " + name;
	}

}