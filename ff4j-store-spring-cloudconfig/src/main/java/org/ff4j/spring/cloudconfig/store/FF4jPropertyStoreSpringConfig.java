package org.ff4j.spring.cloudconfig.store;

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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.springframework.core.env.EnumerablePropertySource;
import org.springframework.core.env.PropertySource;

/**
 * FF4J property store using Spring Cloud Config as data source.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class FF4jPropertyStoreSpringConfig extends AbstractPropertyStore {

    /** Reference to Spring Cloud property provider. */
    private PropertySource<Property < ? > > propertySource;
    
    /**
     * Default constructor.
     *
     * @param propertySource
     */
    public FF4jPropertyStoreSpringConfig(PropertySource< Property < ? > > myPropertySource) {
        this.propertySource = myPropertySource;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return propertySource.containsProperty(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        return (Property<?>) propertySource.getProperty(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map < String , Property< ?> > results = new HashMap<String, Property<?>>();
        for (String propertyName : listPropertyNames()) {
            results.put(propertyName, readProperty(propertyName));
        }
        return results;
    }
    
    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        if (propertySource.getClass().isAssignableFrom(EnumerablePropertySource.class)) {
            return Util.set(((EnumerablePropertySource<?>) propertySource).getPropertyNames());
        }
        return new HashSet<String>();
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        throw new UnsupportedOperationException("Spring property source are readOnly");
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
       throw new UnsupportedOperationException("Spring property source are readOnly");
    }    

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        throw new UnsupportedOperationException("Spring property source are readOnly");
    }
   
}
