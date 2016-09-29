package org.ff4j.test.store;

import java.util.Collection;
import java.util.HashMap;

/*
 * #%L
 * ff4j-test
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


import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.PropertyStore;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Mock implementation of {@link PropertyStore}
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class MockPropertyStore implements PropertyStore {

	/** Logger. */
	private static Logger LOGGER = LoggerFactory.getLogger(MockPropertyStore.class);
	
	/** Custom behaviour. */
    private boolean empty = false;
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        return ("log".equals(name) || "a".equals(name));
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> value) {
    	LOGGER.debug("MOCK [createProperty]");
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        if ("a".equals(name)) return new PropertyString("a", "AMER");
        return null;
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name, Property<?> defaultValue) {
        return readProperty(name);
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
    	LOGGER.debug("MOCK [updateProperty]");
    }

   /** {@inheritDoc} */
    public <T> void updateProperty(Property<T> fixedValue) {
    	LOGGER.debug("MOCK [updateProperty]");
    }

   /** {@inheritDoc} */
    public void deleteProperty(String name) {
    	LOGGER.debug("MOCK [enable]");
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        Map < String, Property<?>> map = new HashMap<String, Property<?>>();
        if (!empty) {
            map.put("a", new PropertyString("a", "AMER"));
        }
        return map;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
    	LOGGER.debug("MOCK [listPropertyNames]");
        return null;
    }

    /** {@inheritDoc} */
    public boolean isEmpty() {
        return false;
    }

    /** {@inheritDoc} */
    public void clear() {
        empty = true;
    }

    /** {@inheritDoc} */
    @Override
    public void importProperties(Collection<Property<?>> properties) {
        LOGGER.debug("MOCK [importProperties]");
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        LOGGER.debug("MOCK [createSchema]");
    }    

}
