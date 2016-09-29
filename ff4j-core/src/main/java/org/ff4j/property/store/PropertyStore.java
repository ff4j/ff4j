package org.ff4j.property.store;

import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.ff4j.property.Property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

/**
 * CRUD repository to perform operation on properties.
 *
 * @author Cedrick Lunven (@clunven)
 */
public interface PropertyStore {
    
    /**
     * Check existence of target property.
     *
     * @param name
     *      target name
     * @return
     *      if the property exist
     */
    boolean existProperty(String name);
    
    /**
     * Create new property within store.
     * 
     * @param value
     *      target value
     */
    <T> void createProperty(Property<T> value);
    
    /**
     * Read property value.
     * 
     * @param name
     *      target property name
     * @return
     *      property of exist
     */
    Property<?> readProperty(String name);
    
    /**
     * Read property value and if not found return the default value
     * 
     * @param name
     *      target property name
     * @return
     *      property of exist
     */
    Property<?> readProperty(String name, Property < ? > defaultValue);
    
    /**
     * Update existing property.
     *
     * @param name
     *      target name
     * @param newValue
     *      new value
     */
    void updateProperty(String name, String newValue);
    
    /**
     * Update existing property.
     *
     * @param name
     *      target name
     * @param newValue
     *      new value
     */
    <T> void updateProperty(Property<T> fixedValue);
    
    /**
     * Delete current property.
     *
     * @param name
     *      target name
     */
    void deleteProperty(String name);
    
    /**
     * Retrieve all properties from store.
     *
     * @return
     *      all properties from store
     */
    Map<String, Property<?> > readAllProperties();
    
    /**
     * List all property names.
     *
     * @return
     */
    Set < String > listPropertyNames();
    
    /**
     * Tell if a store is empty
     * 
     * @return
     *      target store
     */
    boolean isEmpty();
    
    /**
     * Empty current property store.
     *
     */
    void clear();
    
    /**
     * Import a set of properties.
     *
     * @param properties
     *      a set of properties
     */
    void importProperties(Collection<Property<?>> properties);
    
    /**
     * Initialize target database with expected schema if needed.
     */
    void createSchema();
}
