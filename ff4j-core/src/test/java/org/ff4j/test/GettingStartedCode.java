package org.ff4j.test;

/*
 * #%L
 * ff4j-core
 * $Id:$
 * $HeadURL:$
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
import org.ff4j.exception.FeatureNotFoundException;
import org.junit.Test;

/**
 * Those code snipplete are used as samples and eventually propose in documentation
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class GettingStartedCode {
    
    @Test
    public void helloWorld() {

    	FF4j ff4j = new FF4j("ff4j.xml").autoCreate(true);

        // ff4j initialization.....

        if (ff4j.check("sayHello")) {
            // Enhanced Behaviour
            System.out.println("Hello World !");
        } else {
            // Previous Behaviour
        }

    }

    @Test
    public void helloWorld2() {

        FF4j ff4j = new FF4j("ff4j.xml");

        // Work with it
        if (ff4j.check("AwesomeFeature")) {
            // System.out.println("Hello the feature is enabled");
        }

        // Its does not exist
        try {
            if (ff4j.check("do-not-exit")) {}
        } catch (FeatureNotFoundException fnfe) {
            // System.out.println(fnfe.getMessage());
        }

        ff4j.autoCreate(true);
        // no more exception but returning false
        if (ff4j.check("do-not-exit")) {}

    }


}
