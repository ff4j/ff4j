package org.ff4j.store.kv;

/*
 * #%L
 * ff4j-core
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
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

/**
 * Property store to work with key/value.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class KeyValuePropertyStore < VALUE >  extends AbstractPropertyStore {

    /** Driver to access a K/V Store. */
    protected KeyValueDriver < String, VALUE > driver;
    
    /** Work with Mapping. */
    protected PropertyMapper< VALUE > propertyMapper;
    
    /**
     * Default constructor
     */
    public KeyValuePropertyStore() {
    }
            
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValuePropertyStore(KeyValueDriver < String, VALUE > driver) {
        this.driver = driver;
    }
    
    /**
     * Work with Key-Value.
     *
     * @param driver
     *      target driver
     */
    public KeyValuePropertyStore(KeyValueDriver< String, VALUE > driver, PropertyMapper < VALUE > mapper) {
        this.driver = driver;
        this.propertyMapper = mapper;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertParamHasLength(name, "Property name");
        return getDriver().existKey(getDriver().getPropertyKey(name));
    }
    
    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(property.getName())) {
            throw new PropertyAlreadyExistException(property.getName());
        }
        // Create property
        getDriver().putValue(
                getDriver().getPropertyKey(property.getName()), 
                propertyMapper.toStore(property));
        // Register in the dictionnary
        getDriver().registerProperty(property.getName());
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        return getPropertyMapper().fromStore(
                getDriver().getValue(getDriver().getPropertyKey(name)));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        // Delete feature key
        getDriver().deleteKey(getDriver().getPropertyKey(name));
        // Register in the dictionnary
        getDriver().unregisterProperty(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map < String, Property<?>> mapOfProperties = new HashMap<String, Property<?>>();
        for(String propertyName : getDriver().getPropertyList()) {
            Property<?> currP = getPropertyMapper().fromStore(getDriver().getValue(
                                getDriver().getPropertyKey(propertyName)));
            mapOfProperties.put(currP.getName(), currP);
        }
        return mapOfProperties;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return getDriver().getPropertyList();
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        // N+1 select is faster (N+1).log(N) than full scan (N^2) 
        for (String uid : getDriver().getPropertyList()) {
            deleteProperty(uid);
        }
    }

    /**
     * Getter accessor for attribute 'driver'.
     *
     * @return
     *       current value of 'driver'
     */
    public KeyValueDriver< String, VALUE > getDriver() {
        if (driver == null) {
            throw new IllegalStateException("Cannot access target K/V driver, please initialize");
        }
        return driver;
    }

    /**
     * Setter accessor for attribute 'driver'.
     * @param driver
     *      new value for 'driver '
     */
    public void setDriver(KeyValueDriver< String, VALUE > driver) {
        this.driver = driver;
    }

    /**
     * Getter accessor for attribute 'featureMapper'.
     *
     * @return
     *       current value of 'featureMapper'
     */
    public PropertyMapper< VALUE > getPropertyMapper() {
        if (propertyMapper == null) {
            throw new IllegalStateException("Please initialize property mapper");
        }
        return propertyMapper;
    }

    /**
     * Setter accessor for attribute 'featureMapper'.
     * @param featureMapper
     *      new value for 'featureMapper '
     */
    public void setPropertyMapper(PropertyMapper<VALUE> mapper) {
        this.propertyMapper = mapper;
    }   

}
