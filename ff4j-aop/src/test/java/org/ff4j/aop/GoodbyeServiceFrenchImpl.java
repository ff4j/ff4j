package org.ff4j.aop;

import org.springframework.stereotype.Component;

@Component("goodbye.french")
public class GoodbyeServiceFrenchImpl implements GoodbyeService {

	@Override
	public String sayGoodbye(String name) {
		return "Au revoir " + name;
	}

	@Override
	public String sayGoodbyeWithClass(String name) {
		return "A plus " + name;
	}
}