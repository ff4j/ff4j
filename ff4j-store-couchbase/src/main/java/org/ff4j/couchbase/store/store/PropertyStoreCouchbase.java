package org.ff4j.couchbase.store.store;

/*
 * #%L
 * ff4j-store-springcouchbase
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

import com.couchbase.client.java.Bucket;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.ff4j.couchbase.store.document.PropertyDocument;
import org.ff4j.couchbase.store.mapper.DocumentMapper;
import org.ff4j.couchbase.store.mapper.ObjectMapperFactory;
import org.ff4j.couchbase.store.repository.CouchbaseRepository;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

/**
 * Created by farrellyja on 10/11/2017.
 */
public class PropertyStoreCouchbase extends AbstractPropertyStore {

    /** Couchbase bucket connections **/
    private CouchbaseRepository<PropertyDocument> propertyRepository;

    /**
     * Default constructor.
     */
    public PropertyStoreCouchbase(Bucket propertyBucket) {
        ObjectMapper objectMapper = ObjectMapperFactory.createMapper();
        this.propertyRepository = new CouchbaseRepository<>(propertyBucket, PropertyDocument.class, objectMapper);
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {

    }

    /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return propertyRepository.get(name) != null;
    }

    /** {@inheritDoc} */
    @Override
    public <T> void createProperty(Property<T> prop) {
        assertPropertyNotNull(prop);
        assertPropertyNotExist(prop.getName());
        if (prop.getFixedValues() != null && !prop.getFixedValues().isEmpty() && !prop.getFixedValues().contains(prop.getValue())) {
            throw new IllegalArgumentException("Value " + prop.getValue() + " is not within fixed values " + prop.getFixedValues());
        }
        propertyRepository.upsert(prop.getName(), DocumentMapper.propertyToPropertyDocument(prop));
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        return DocumentMapper.propertyDocumentToProperty(propertyRepository.get(name));
    }

    /** {@inheritDoc} */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        propertyRepository.remove(name);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        return propertyRepository.getAll().stream()
                .map(DocumentMapper::propertyDocumentToProperty)
                .collect(Collectors.toMap(Property::getName, p -> p));
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> listPropertyNames() {
        return propertyRepository.getAll().stream()
                .map(PropertyDocument::getName)
                .collect(Collectors.toSet());
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        propertyRepository.removeAll();
    }
}
