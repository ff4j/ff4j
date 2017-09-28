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

import java.util.Map;

import org.ff4j.FF4j;
import org.ff4j.property.Property;
import org.springframework.cloud.config.environment.PropertySource;
import org.springframework.core.env.EnumerablePropertySource;

/**
 * Implementation of spring config {@link PropertySource} to inject properties from FF4j (property Store). 
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jPropertySource extends EnumerablePropertySource<Map<String, Property<?>>> {
   
    /**  FF4j as source for Spring Config. */
    private static final String PROPERTY_SOURCE_NAME = "FF4JSource";
    
    /** Reference to FF4J. */
    private FF4j ff4j;
    
    /**
     * Expected constructor to populate {@link FF4j}
     *
     * @param ff4j
     *      current instance of {@link FF4j}
     */
    public FF4jPropertySource(FF4j ff4j) {
        this(PROPERTY_SOURCE_NAME);
        this.ff4j = ff4j;
    }
    
    /**
     * Contructor with Parameters (protected not visible)
     *
     * @param name
     *      unium
     */
    protected FF4jPropertySource(String name) {
        super(name);
    }

    /** {@inheritDoc} */
    @Override
    public String[] getPropertyNames() {
        return ff4j.getPropertiesStore().listPropertyNames().toArray(new String[0]);
    }
    
    /** {@inheritDoc} */
    @Override
    public Object getProperty(String name) {
        if (!ff4j.getPropertiesStore().existProperty(name)) return null;        
        Property<?> p = ff4j.getPropertiesStore().readProperty(name);
        return p;
    }
    
}
