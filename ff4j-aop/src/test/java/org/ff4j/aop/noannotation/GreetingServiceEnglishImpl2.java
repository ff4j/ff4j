package org.ff4j.aop.noannotation;

public class GreetingServiceEnglishImpl2 implements GreetingService2 {
    
    @Override
	public String sayHello(String name) {return "Hello " + name;}

	@Override
	public String sayHelloWithClass(String name) {
		return "Hi " + name;
	}

}