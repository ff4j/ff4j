package org.ff4j.property;

import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;
import static org.ff4j.utils.JsonUtils.attributeAsJson;
import static org.ff4j.utils.JsonUtils.collectionAsJson;

import java.io.InputStream;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import java.util.stream.Stream;

import org.ff4j.conf.XmlConfigV1;
import org.ff4j.conf.XmlParserV1;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.monitoring.AuditTrail;
import org.ff4j.repository.FF4jRepositoryListener;
import org.ff4j.repository.FF4jRepositorySupport;


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
public abstract class AbstractRepositoryProperties 
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
    public  Map<String, Property<?>> importPropertiesFromXmlFile(String xmlConfFile) {
        // Argument validation
        if (xmlConfFile == null || xmlConfFile.isEmpty()) {
            throw new IllegalArgumentException("Configuration filename cannot be null nor empty");
        }
        // Load as Inputstream
        InputStream xmlIS = getClass().getClassLoader().getResourceAsStream(xmlConfFile);
        if (xmlIS == null) {
            throw new IllegalArgumentException("File " + xmlConfFile + " could not be read, please check path and rights");
        }
        // Use the Feature Parser
        XmlConfigV1 conf = new XmlParserV1().parseConfigurationFile(xmlIS);
        Map<String, Property<?>> properties = conf.getProperties();

        // Override existing configuration within database
        for (Map.Entry<String,Property<?>> featureName : properties.entrySet()) {
            if (exists(featureName.getKey())) {
                delete(featureName.getKey());
            }
            create(featureName.getValue());
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
    public void update(Property<?> prop) {
        assertNotNull(prop);
        assertHasLength(prop.getUid());
        // Delete
        delete(prop.getUid());
        // Create
        create(prop);
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
        update(p);
    }
    
    /** {@inheritDoc} */
    @Override
    public void save(Collection<Property<?>> properties) {
        // Do not use target as the delete/create operation will be traced
        if (properties != null) {
            for (Property<?> property : properties) {
                if (exists(property.getUid())) {
                    delete(property.getUid());
                }
                create(property);
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> read(String name) {
        assertPropertyExist(name);
        return findById(name).get();
    }       
    
    /** {@inheritDoc} */
    @Override
    public Property<?> read(String name, Property < ? > defaultValue) {
        return findById(name).orElse(defaultValue);
    }
    
    /** {@inheritDoc} */
    @Override
    public void delete(Iterable<? extends Property<?>> entities) {
        if (null != entities) {
            entities.forEach(this::delete);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void delete(Property<?> entity) {
        assertPropertyExist(entity.getUid());
        delete(entity.getUid());
    }

    /** {@inheritDoc} */
    @Override
    public Stream<Property<?>> find(Iterable<String> candidates) {
        if (candidates == null) return null;
        List < Property<?> > targets  = new ArrayList<>();
        candidates.forEach(id -> targets.add(read(id)));
        return targets.stream();
    }
    
    /** {@inheritDoc} */
    @Override
    public void registerListener(String name, FF4jRepositoryListener<Property<?>> listener) {
        // Enforce subclass to reach AbstractObservable.registerListener(..)
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
