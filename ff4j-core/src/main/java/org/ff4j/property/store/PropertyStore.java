package org.ff4j.property.store;

import java.util.Map;

import org.ff4j.property.AbstractProperty;

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
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
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
    <T> void createProperty(AbstractProperty<T> value);
    
    /**
     * Read property value.
     * 
     * @param name
     *      target property name
     * @return
     *      property of exist
     */
    AbstractProperty<?> readProperty(String name);
    
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
    <T> void updateProperty(AbstractProperty<T> fixedValue);
    
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
    Map<String, AbstractProperty<?> > readAllProperties();

}
