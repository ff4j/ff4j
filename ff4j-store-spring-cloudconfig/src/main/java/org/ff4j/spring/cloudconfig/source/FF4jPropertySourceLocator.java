package org.ff4j.spring.cloudconfig.source;

/*
 * #%L
 * ff4j-store-spring-cloudconfig
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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
import org.springframework.cloud.bootstrap.config.PropertySourceLocator;
import org.springframework.core.env.Environment;
import org.springframework.core.env.PropertySource;

/**
 * Provide a bridge to load FF4J properties as source for Spring Cloud Config.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jPropertySourceLocator implements PropertySourceLocator {
  
    /** Instance of FF4J to be used. */
    private FF4j ff4j;
    
    public FF4jPropertySourceLocator(FF4j ff4j) {
        this.ff4j = ff4j;
    }
    
    /** {@inheritDoc} */
    @Override
    public PropertySource<?> locate(Environment environment) {
        return new FF4jPropertySource(ff4j);
    }

}
