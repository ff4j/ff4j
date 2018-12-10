package org.ff4j.property.repository;

import java.util.stream.Stream;

import org.ff4j.FF4jRepository;
import org.ff4j.exception.ItemNotFoundException;
import org.ff4j.feature.exception.FeatureNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyNotFoundException;

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
public interface PropertyRepository extends FF4jRepository < String, Property<?> > {
    
    /**
     * Syntax sugar.
     *
     * @return
     *      list of properties names.
     */
    default Stream < String > listPropertyNames() {
        return findAllIds();
    }
    
    /**
     * Update value from String.
     * 
     * @param name
     *          current property name
     * @param newValue
     *          new value as a String
     */
    default void update(String name, String newValue) {
        Property<?> p = read(name);
        p.setValueFromString(newValue);
        saveProperty(p);
    }
    
    /**
     * Specialization of 'update' without any concern about audit nor cache.
     *
     * @param fp
     *      target feature
     */
    void saveProperty(Property<?> property);
    
    /**
     * Specialization of 'delete' without any concern about audit nor cache.
     *
     * @param fp
     *      target feature
     */
    void deleteProperty(String uid);
    
    /**
     * Specializing exception as {@link FeatureNotFoundException}
     */
    @Override
    default Property<?> read(String uid) {
        try {
            return FF4jRepository.super.read(uid);
        } catch(ItemNotFoundException inf) {
            throw new PropertyNotFoundException(uid);
        }
    }
}
