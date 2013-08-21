package org.ff4j.aop;

public interface GreetingService {

	@Flip(name = "language-french", alterBean = "greeting.french")
	String sayHello(String name);

	@Flip(name = "language-french", alterClazz = GreetingServiceFrenchImpl.class)
	String sayHelloWithClass(String name);

}