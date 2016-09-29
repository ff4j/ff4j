package org.ff4j.elastic.store;

import java.util.Map;
import java.util.Set;

import org.ff4j.elastic.ElasticConnection;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;

/*
 * #%L
 * ff4j-store-elastic
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


public class PropertyStoreElastic extends AbstractPropertyStore {

    private ElasticConnection connection;
    
    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        // TODO Auto-generated method stub
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> value) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        // TODO Auto-generated method stub
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }

    /**
     * Getter accessor for attribute 'connection'.
     *
     * @return
     *       current value of 'connection'
     */
    public ElasticConnection getConnection() {
        return connection;
    }

    /**
     * Setter accessor for attribute 'connection'.
     * @param connection
     * 		new value for 'connection '
     */
    public void setConnection(ElasticConnection connection) {
        this.connection = connection;
    }

}
