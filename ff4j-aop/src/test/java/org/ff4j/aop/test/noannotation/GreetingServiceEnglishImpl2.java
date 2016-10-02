package org.ff4j.aop.test.noannotation;

import org.ff4j.aop.Flip;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


public class GreetingServiceEnglishImpl2 implements GreetingService2 {
    
    @Override
	public String sayHello(String name) {return "Hello " + name;}

	@Override
	public String sayHelloWithClass(String name) {
		return "Hi " + name;
	}
	
	@Flip(name = "language-french", alterClazz = GreetingServiceGermanImpl.class)
    public String sayGerman() {
	    return "Salut";
	}

    @Override
    public String sayHallow(String name) {
        return sayGerman();
    }

}