package org.ff4j.test.store;

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

public class MockPropertyStore implements PropertyStore {

    private boolean empty = false;
    @Override
    public boolean existProperty(String name) {
        if ("log".equals(name) || "a".equals(name)) return true;
        return false;
    }

    @Override
    public <T> void createProperty(Property<T> value) {
    }

    @Override
    public Property<?> readProperty(String name) {
        if ("a".equals(name)) return new PropertyString("a", "AMER");
        return null;
    }

    @Override
    public void updateProperty(String name, String newValue) {
    }

    @Override
    public <T> void updateProperty(Property<T> fixedValue) {
    }

    @Override
    public void deleteProperty(String name) {
    }

    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map < String, Property<?>> map = new HashMap<String, Property<?>>();
        if (!empty) {
            map.put("a", new PropertyString("a", "AMER"));
        }
        return map;
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
        empty = true;
    }

}
