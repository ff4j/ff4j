package org.ff4j.aop;

/*-
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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
import org.junit.jupiter.api.AfterEach;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit.jupiter.SpringExtension;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.junit.jupiter.api.Assertions.assertThrows;

@ExtendWith(SpringExtension.class)
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

    @BeforeEach
    public void createFeatures() {
        if (!ff4j.exist("language-english")) {
            ff4j.createFeature("language-english");
        }
        if (!ff4j.exist("language-french")) {
            ff4j.createFeature("language-french");
        }
    }

    @AfterEach
    public void disableFeatures() {
        ff4j.disable("language-french");
        ff4j.disable("language-english");
    }

    @Test
    public void testAnnotatedFlippingwithalterBean() {
        ff4j.disable("language-french");
        Assertions.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));

        ff4j.enable("language-french");
        Assertions.assertTrue(greeting.sayHello("CLU").startsWith("Bonjour"), "Service did not flipped");
    }

    @Test
    @Disabled
    public void testAnnotatedFlippingwithalterClazz() {
        Assertions.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Hi"));
        ff4j.enable("language-french");
        Assertions.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Salut"), "Service did not flipped");
    }

    @Test
    public void testAnnotatedFlippingifqualifiedimplementationisnotthefirstclassqualifiednameinnaturalordering() {
        Assertions.assertTrue(goodbye.sayGoodbye("CLU").startsWith("Au revoir"));
        ff4j.enable("language-english");
        Assertions.assertTrue(goodbye.sayGoodbye("CLU").startsWith("Goodbye"), "Service did not flipped");
    }

    @Test
    @Disabled
    public void testAnnotatedFlippingwithalterClazzifqualifiedimplementationisnotthefirstclassqualifiednameinnaturalordering() {
        Assertions.assertTrue(goodbye.sayGoodbyeWithClass("CLU").startsWith("A plus"));
        ff4j.enable("language-english");
        Assertions.assertTrue(goodbye.sayGoodbyeWithClass("CLU").startsWith("See you"), "Service did not flipped");
    }

    @Test
    public void testAlterBeanInvokeThrowInvocationTargetExceptionNull() throws Exception {
        assertThrows(IllegalArgumentException.class, () -> {
            ff4j.enable("language-english");
            goodbye.sayGoodbyeInvocationTargetExceptionNull();
        });
    }

    @Test
    public void testAlterClazzInvokeThrowInvocationTargetExceptionNull() throws InvocationTargetException {
        assertThrows(IllegalArgumentException.class, () -> {
            ff4j.enable("language-english");
            goodbye.sayGoodbyeWithClassInvocationTargetExceptionNull();
        });
    }

    @Test
    public void testNamespace() throws Exception {
        Constructor<FF4jNameSpaceConstants> c = FF4jNameSpaceConstants.class.getDeclaredConstructor();
        c.setAccessible(true);
        Assertions.assertNotNull(c.newInstance());
    }
}
