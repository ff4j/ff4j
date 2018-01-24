package org.ff4j.aop.test.noalterbean;

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
import org.ff4j.test.AssertFf4j;
import org.junit.Assert;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;
import org.springframework.test.context.junit4.SpringJUnit4ClassRunner;

@RunWith(SpringJUnit4ClassRunner.class)
@ContextConfiguration("classpath:applicationContext-ff4j-aop-test.xml")
public class SampleServiceFeatureTest {

    @Autowired
    private FF4j ff4j;

    @Autowired
    private SampleService sample;
    
    private AssertFf4j assertFF4j;

    @Test
    public void testAOP() {
    	
    	// Given
    	ff4j.createFeature("AwesomeFeature", true);
    	assertFF4j = new AssertFf4j(ff4j);
    	assertFF4j.assertThatFeatureExist("AwesomeFeature");
    	assertFF4j.assertThatFeatureIsEnabled("AwesomeFeature");
    	
    	// When feature is enabled 
    	String result1 = sample.someAwesomeFeature("HELLO");
    	// Then it works as expected
    	Assert.assertEquals("HELLO", result1);
    	
    	// When feature is disabled, nothing and return null
    	ff4j.disable("AwesomeFeature");
    	
    	// Then
    	Assert.assertNull(sample.someAwesomeFeature("HELLO"));
    	
    }

}
