package org.ff4j.aop.test.context;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import org.ff4j.core.FlippingExecutionContext;
import org.springframework.stereotype.Component;

@Component("context.english")
public class ContextServiceEnglishImpl implements ContextService {
    @Override
    public String sayHelloWithThreadLocal(String name) {
        return "Hello " + name;
    }

    @Override
    public String sayHelloWithParameter(String name, FlippingExecutionContext context) {
        return "Hello " + name;
    }
}
