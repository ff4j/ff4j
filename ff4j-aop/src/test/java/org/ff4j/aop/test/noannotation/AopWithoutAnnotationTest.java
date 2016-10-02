package org.ff4j.aop.test.noannotation;

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
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-no-annotation.xml")
public class AopWithoutAnnotationTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("greeting.noannotation.english")
    private GreetingService2 greeting;
    
    @Test
    public void testAOP() {
        ff4j.disable("language-french");
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
        ff4j.enable("language-french");
        
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Bonjour"));
        ff4j.disable("language-french");
        
        Assert.assertTrue(greeting.sayHello("CLU").startsWith("Hello"));
    }
    
    @Test
    public void testAOPAlterClassSpring() {
        Assert.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Hi"));
        ff4j.enable("language-french");
        Assert.assertTrue(greeting.sayHelloWithClass("CLU").startsWith("Salut"));
    }
    
    @Test
    public void testAOPAlterClass() {
        Assert.assertTrue(greeting.sayHallow("CLU").startsWith("Salut"));
        ff4j.enable("language-german");
        Assert.assertTrue(greeting.sayHallow("CLU").startsWith("Hallo"));
    }

}
