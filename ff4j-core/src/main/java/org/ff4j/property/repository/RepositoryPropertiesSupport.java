package org.ff4j.property.repository;

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.attributeAsJson;
import static org.ff4j.utils.JsonUtils.collectionAsJson;

import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

import org.ff4j.FF4jRepositoryListener;
import org.ff4j.FF4jRepositorySupport;
import org.ff4j.event.repository.AuditTrail;
import org.ff4j.parser.xml.XmlParserV2;
import org.ff4j.property.Property;
import org.ff4j.property.exception.PropertyAlreadyExistException;
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
 * Superclass for any property store.
 *
 * @author Cedrick Lunven (@clunven)
 */
public abstract class RepositoryPropertiesSupport 
    extends FF4jRepositorySupport<Property<?>, RepositoryPropertiesListener> 
    implements RepositoryProperties {
    
    /** serialVersionUID. */
    private static final long serialVersionUID = -5638535944745337074L;
    
    /** Listener Name. */
    private static final String LISTENERNAME_AUDIT = "PropertyStoreAuditListener";

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        /* 
         * In most of cases there is nothing to do. The feature and properties are createdat runtime.
         * But not always (JDBC, Mongo, Cassandra)... this is the reason why the dedicated store must 
         * override this method. It a default implementation (Pattern Adapter).
         */
        return;
    }
    
    /**
     * Initialize store from XML Configuration File.
     *
     * @param xmlConfFile
     *      xml configuration file
     */
    public  Map<String, ? extends Property<?>> importPropertiesFromXmlFile(String xmlConfFile) {
        Map<String, ? extends Property<?>> properties = new XmlParserV2().parse(xmlConfFile).getProperties();
        for (Map.Entry<String,? extends Property<?>> featureName : properties.entrySet()) {
            if (exists(featureName.getKey())) {
                delete(featureName.getKey());
            }
            save(featureName.getValue());
        }
        return properties;
    }
    
    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append(attributeAsJson("type", this.getClass().getCanonicalName()));
        
        Set<String> props = findAll().map(Property::getUid).collect(Collectors.toSet());
        sb.append(",\"numberOfProperties\":" + props.size());
        sb.append(",\"properties\":" + collectionAsJson(props));
        
        sb.append("}");
        return sb.toString();
    }
    
    /**
     * Validate property name and existence
     *
     * @param uid
     *      target uid
     */
    protected void assertPropertyExist(String name) {
        assertHasLength(name);
        if (!exists(name)) {
            throw new PropertyNotFoundException(name);
        }
    }
    
    /**
     * Check that current feature does not exist.
     *
     * @param uid
     *      current feature identifier.s
     */
    protected void assertPropertyNotExist(String uid) {
        assertHasLength(uid);
        if (exists(uid)) {
            throw new PropertyAlreadyExistException(uid);
        }
    }
    
    /**
     * Validate feature uid.
     *
     * @param uid
     *      target uid
     */
    protected void assertPropertyNotNull(Property<?> property) {
        if (property == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> read(String name) {
        assertPropertyExist(name);
        return find(name).get();
    } 
    
    /** {@inheritDoc} */
    @Override
    public void save(Iterable<Property<?>> entities) {
        assertNotNull(entities);
        entities.forEach(this::saveProperty);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(Iterable<String> entities) {
        assertNotNull(entities);
        entities.forEach(this::deleteProperty);
    }
   
    /** {@inheritDoc} */
    @Override
    public void update(String name, String newValue) {
        assertHasLength(name);
        // Read feature not found if no present
        Property<?> p = read(name);
        // Update within Object
        p.setValueFromString(newValue);
        // Serialization and update key, update TTL
        saveProperty(p);
    }
    
    // --------------------------
    //         Specialisation
    // --------------------------
    
    /**
     * Unitary operation.
     *
     * @param prop
     *      current property
     */
    protected abstract void saveProperty(Property<?> prop);
    
    /**
     * Delete operation.
     *
     * @param prop
     *      current property
     */
    protected abstract void deleteProperty(String propertyId);
    
    // --------------------------
    //         Listeners
    // --------------------------
    
    /** {@inheritDoc} */
    @Override
    public void registerListener(String name, FF4jRepositoryListener<Property<?>> listener) {
        registerListener(name, (RepositoryPropertiesListener) listener);
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerAuditListener(AuditTrail auditTrail) {
        this.registerListener(LISTENERNAME_AUDIT, new RepositoryPropertiesListenerAudit(auditTrail));
    }
    
    /** {@inheritDoc} */
    @Override
    public void unRegisterAuditListener() {
        this.unregisterListener(LISTENERNAME_AUDIT);
    }
    
}
