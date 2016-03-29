package org.ff4j.aop.test.goodbye;

import org.ff4j.aop.Flip;

import java.lang.reflect.InvocationTargetException;

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

public interface GoodbyeService {

	@Flip(name = "language-english", alterBean = "goodbye.english")
	String sayGoodbye(String name);

	@Flip(name = "language-english", alterClazz = GoodbyeServiceEnglishImpl.class)
	String sayGoodbyeWithClass(String name);

	@Flip(name = "language-english", alterBean = "goodbye.english")
	void sayGoodbyeThrowException();

	@Flip(name = "language-english", alterClazz = GoodbyeServiceEnglishImpl.class)
	void sayGoodbyeWithClassThrowException();

	@Flip(name = "language-english", alterBean = "goodbye.english")
	void sayGoodbyeInvocationTargetExceptionNull() throws InvocationTargetException;

	@Flip(name = "language-english", alterClazz = GoodbyeServiceEnglishImpl.class)
	void sayGoodbyeWithClassInvocationTargetExceptionNull() throws InvocationTargetException;

}