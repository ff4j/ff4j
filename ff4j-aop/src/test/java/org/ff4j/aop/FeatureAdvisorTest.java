package org.ff4j.aop;

import java.lang.reflect.Constructor;
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

import org.ff4j.FF4j;
import org.ff4j.aop.test.goodbye.GoodbyeService;
import org.ff4j.aop.test.greeting.GreetingService;
import org.ff4j.spring.namespace.FF4jNameSpaceConstants;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class FeatureAdvisorTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("greeting.english")
    private GreetingService greeting;

    @Autowired
    @Qualifier("goodbye.french")
    private GoodbyeService goodbye;

    @Before
    public void createFeatures() {
        if (!ff4j.exist("language-english")) {
            ff4j.createFeature("language-english");
        }
        if (!ff4j.exist("language-french")) {
            ff4j.createFeature("language-french");
        }
    }

    @After
    public void disableFeatures() {
        ff4j.disable("language-french");
        ff4j.disable("language-english");
    }

    @Test
    public void testAnnotatedFlippingwithalterBean() {
        ff4j.disable("language-french");
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));

        ff4j.enable("language-french");
        Assert.assertTrue("Service did not flipped", greeting.sayHello("CLU").startsWith("Bonjour"));
    }

    @Test
    @Ignore
    public void testAnnotatedFlippingwithalterClazz() {
        Assert.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Hi"));
        ff4j.enable("language-french");
        Assert.assertTrue("Service did not flipped", greeting.sayHelloWithClass("CLU").startsWith("Salut"));
    }

    @Test
    public void testAnnotatedFlippingifqualifiedimplementationisnotthefirstclassqualifiednameinnaturalordering() {
        Assert.assertTrue(goodbye.sayGoodbye("CLU").startsWith("Au revoir"));
        ff4j.enable("language-english");
        Assert.assertTrue("Service did not flipped", goodbye.sayGoodbye("CLU").startsWith("Goodbye"));
    }

    @Test
    @Ignore
    public void testAnnotatedFlippingwithalterClazzifqualifiedimplementationisnotthefirstclassqualifiednameinnaturalordering() {
        Assert.assertTrue(goodbye.sayGoodbyeWithClass("CLU").startsWith("A plus"));
        ff4j.enable("language-english");
        Assert.assertTrue("Service did not flipped", goodbye.sayGoodbyeWithClass("CLU").startsWith("See you"));
    }

    @Test(expected = IllegalArgumentException.class)
    public void testAlterBeanInvokeThrowInvocationTargetExceptionNull() throws Exception {
        ff4j.enable("language-english");
        goodbye.sayGoodbyeInvocationTargetExceptionNull();
    }

    @Test(expected = IllegalArgumentException.class)
    public void testAlterClazzInvokeThrowInvocationTargetExceptionNull() throws InvocationTargetException {
        ff4j.enable("language-english");
        goodbye.sayGoodbyeWithClassInvocationTargetExceptionNull();
    }
    
    @Test
    public void testNamespace() throws Exception {
        Constructor<FF4jNameSpaceConstants> c = FF4jNameSpaceConstants.class.getDeclaredConstructor();
        c.setAccessible(true);
        Assert.assertNotNull(c.newInstance());
    }
}