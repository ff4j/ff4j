package org.ff4j.aop;

public interface GoodbyeService {

	@Flip(name = "language-english", alterBean = "goodbye.english")
	String sayGoodbye(String name);

	@Flip(name = "language-english", alterClazz = GoodbyeServiceEnglishImpl.class)
	String sayGoodbyeWithClass(String name);

}