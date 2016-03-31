package org.ff4j.aop.test.goodbye;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 Ff4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import org.ff4j.aop.test.exception.ApplicationException;
import org.springframework.stereotype.Component;

import java.lang.reflect.InvocationTargetException;

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

	@Override
	public void sayGoodbyeThrowException() {
		throw new ApplicationException();
	}

	@Override
	public void sayGoodbyeWithClassThrowException() {
		throw new ApplicationException();
	}

	@Override
	public void sayGoodbyeInvocationTargetExceptionNull() throws InvocationTargetException {
		throw new InvocationTargetException(null);
	}

	@Override
	public void sayGoodbyeWithClassInvocationTargetExceptionNull() throws InvocationTargetException {
		throw new InvocationTargetException(null);
	}
}