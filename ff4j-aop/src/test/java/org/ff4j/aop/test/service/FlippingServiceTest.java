package org.ff4j.aop.test.service;

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
public class FlippingServiceTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    @Qualifier("whole.english.service")
    private FlippingServiceStereotype service;

    /**
     * TDD
     */
    @Test
    public void testAOPClass() {
        // Given english mode
        Assert.assertTrue(service.hello1().startsWith("Hello"));
        Assert.assertTrue(service.hello2().startsWith("Big"));
        // when
        ff4j.enable("language-french");
        // Then
        Assert.assertTrue(service.hello1().startsWith("Francais"));
        Assert.assertTrue(service.hello2().startsWith("Tour"));
    }

    @After
    public void disable() {
        ff4j.disable("language-french");
    }

}
