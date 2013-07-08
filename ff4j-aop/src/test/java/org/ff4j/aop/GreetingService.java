package org.ff4j.aop;

public interface GreetingService {
	
	@Flip(name="language-french", alterBean="greeting.french")
	String sayHello(String name);
	
}
