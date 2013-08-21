package org.ff4j.aop;

import org.springframework.stereotype.Component;

@Component("goodbye.english")
public class GoodbyeServiceEnglishImpl implements GoodbyeService {

	@Override
	public String sayGoodbye(String name) {
		return "Goodbye " + name;
	}

	@Override
	public String sayGoodbyeWithClass(String name) {
		return "See you " + name;
	}
}