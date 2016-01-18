package org.ff4j.aop.tag;

import org.ff4j.aop.ContextLocation;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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


import org.ff4j.spring.autowire.AutowiredFF4JBeanPostProcessor;
import org.junit.Assert;
import org.junit.Test;
import org.springframework.context.support.ClassPathXmlApplicationContext;

public class FF4JBeanPostProcessorTest {
    
    @Test
    public void testPostProcessorNullBean() {
        AutowiredFF4JBeanPostProcessor pp = new AutowiredFF4JBeanPostProcessor();
        Assert.assertNull(pp.postProcessAfterInitialization(null,null));
    }
    
    @Test
    public void testFF4JTagOK() {
        Assert.assertNotNull(checkFile("springctx-ff4jtags-1.xml").getBean("ff4j"));
    }
    
    @Test
    public void testFF4JTagEmpty() {
        Assert.assertNotNull(checkFile("springctx-ff4jtags-2.xml").getBean("ff4j"));
    }
    
    @Test
    public void testFF4JTagAuthorisationManager() {
        Assert.assertNotNull(checkFile("springctx-ff4jtags-3.xml").getBean("ff4j"));
    }
    
    @Test
    public void contextLocationparsing() {
        Assert.assertNotNull(ContextLocation.valueOf("NONE"));
    }
    
    private ClassPathXmlApplicationContext checkFile(String fileName) {
        return new ClassPathXmlApplicationContext(fileName);
    }

}
