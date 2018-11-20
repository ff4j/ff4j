package org.ff4j.property.repo;

import java.util.stream.Stream;

import org.ff4j.property.Property;
import org.ff4j.repository.FF4jRepository;

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
public interface RepositoryProperties extends FF4jRepository < String, Property<?> > {
    
    /**
     * Update existing property from a string value.
     *
     * @param name
     *      target name
     * @param newValue
     *      new value
     */
    void update(String name, String newValue);
    
    /**
     * Syntax sugar.
     *
     * @return
     *      list of properties names.
     */
    default Stream < String > listPropertyNames() {
        return findAllIds();
    }
}
