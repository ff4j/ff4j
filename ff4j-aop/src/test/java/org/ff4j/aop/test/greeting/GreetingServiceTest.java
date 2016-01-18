package org.ff4j.aop.test.greeting;

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
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import org.ff4j.FF4j;
import org.ff4j.aop.test.context.ContextService;
import org.ff4j.core.FlippingExecutionContext;
import org.junit.After;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class GreetingServiceTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("greeting.english")
    private GreetingService greeting;

    @Autowired
    @Qualifier("context.english")
    private ContextService context;

    @Test
    public void testAOP() {
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
        ff4j.enable("language-french");
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Bonjour"));
    }

    @Test
    public void testAOPWithParameter() {
        ff4j.enable("context-french");

        Assert.assertTrue(context.sayHelloWithParameter("CLU", null).startsWith("Hello"));

        FlippingExecutionContext executionContext = new FlippingExecutionContext();

        executionContext.putString("user.settings.language", "english");
        Assert.assertTrue(context.sayHelloWithParameter("CLU", executionContext).startsWith("Hello"));

        executionContext.putString("user.settings.language", "french");
        Assert.assertTrue(context.sayHelloWithParameter("CLU", executionContext).startsWith("Bonjour"));
    }

    @Test
    public void testAOPWithThreadLocal() {
        ff4j.enable("context-french");

        FlippingExecutionContext executionContext = ff4j.getCurrentContext();

        executionContext.putString("user.settings.language", "english");
        Assert.assertTrue(context.sayHelloWithThreadLocal("CLU").startsWith("Hello"));

        executionContext.putString("user.settings.language", "french");
        Assert.assertTrue(context.sayHelloWithThreadLocal("CLU").startsWith("Bonjour"));
    }

    @After
    public void disable() {
        ff4j.disable("language-french");
    }
}
