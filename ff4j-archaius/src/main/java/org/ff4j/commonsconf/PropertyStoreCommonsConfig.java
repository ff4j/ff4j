package org.ff4j.commonsconf;

/*
 * #%L
 * ff4j-archaius
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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

import org.apache.commons.configuration.Configuration;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;

/**
 * Create {@link PropertyStore} from commons-conf.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreCommonsConfig extends AbstractPropertyStore {

    /** Internal commons configuration. */
    private Configuration configuration;
    
    /**
     * Default constructor allowing IOC.
     */
    public PropertyStoreCommonsConfig() {
    }
    
    /**
     * Initialisation woth contructor.
     * 
     * @param configuration
     *      commons configuration generic config {@link Configuration}
     */
    public PropertyStoreCommonsConfig(Configuration configuration) {
        this.configuration = configuration;
    }
    
    /**
     * Read not null configuration.
     *
     * @return
     *      target configuration
     */
    private Configuration conf() {
        if (configuration == null) {
            throw new IllegalStateException("Configuration has not been initialized yet.");
        }
        return configuration;
    }
    
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return conf().containsKey(name);
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        conf().clearProperty(name);
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        Set < String > propertyNames = new HashSet<String>();
        Iterator< String> keysIter = configuration.getKeys();
        while(keysIter.hasNext()) {
            propertyNames.add(keysIter.next());
        }
        return propertyNames;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
        Util.assertNotNull(value);
        Util.hasLength(value.getName());
        if (existProperty(value.getName())) {
            throw new PropertyAlreadyExistException(value.getName());
        }
        conf().addProperty(value.getName(), value.asString());
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        Util.assertHasLength(name);
        String value = conf().getString(name);
        if (value == null) {
            throw new PropertyNotFoundException(name);
        }
        return new PropertyString(name, value);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> props = new HashMap<String, Property<?>>();
        Iterator<String > iterKeys = conf().getKeys();
        while(iterKeys.hasNext()) {
            String currentKey = iterKeys.next();
            props.put(currentKey, readProperty(currentKey));
        }
        return props;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        conf().clear();
    }

    /**
     * Getter accessor for attribute 'configuration'.
     *
     * @return
     *       current value of 'configuration'
     */
    public Configuration getConfiguration() {
        return configuration;
    }

    /**
     * Setter accessor for attribute 'configuration'.
     * @param configuration
     * 		new value for 'configuration '
     */
    public void setConfiguration(Configuration configuration) {
        this.configuration = configuration;
    }       
}
