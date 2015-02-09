package org.ff4j.property.store;

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
 * Properties.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public interface PropertyStore {
    
    /**
     * Create new property within store.
     * 
     * @param value
     *      target value
     */
    <T> void create(AbstractProperty<T> value);
    
    /**
     * Read property value.
     * 
     * @param name
     *      target property name
     * @return
     *      property of exist
     */
    AbstractProperty<?> read(String name);
    
    /**
     * Update existing property.
     *
     * @param name
     *      target name
     * @param newValue
     *      new value
     */
    <T> void update(String name, AbstractProperty<T> newValue);
    
    /**
     * Delete current property.
     *
     * @param name
     *      target name
     */
    void delete(String name);

}
