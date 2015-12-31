package org.ff4j.jmx.gateway;

/*
 * #%L
 * ff4j-jmx
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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

import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.PropertyStore;

/**
 * Implementation of PropertyStore to operate remotely.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreJmxGateway implements PropertyStore {

    @Override
    public boolean existProperty(String name) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public <T> void createProperty(AbstractProperty<T> value) {
    }

    @Override
    public AbstractProperty<?> readProperty(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void updateProperty(String name, String newValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public <T> void updateProperty(AbstractProperty<T> fixedValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void deleteProperty(String name) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, AbstractProperty<?>> readAllProperties() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Set<String> listPropertyNames() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public boolean isEmpty() {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

}
