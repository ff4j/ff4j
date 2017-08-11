package org.ff4j.property.store;

import java.io.InputStream;
import java.util.Collection;
import java.util.Map;
import java.util.Set;

import org.ff4j.conf.XmlConfig;
import org.ff4j.conf.XmlParser;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;

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
public abstract class AbstractPropertyStore implements PropertyStore {
    
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
        XmlConfig conf = new XmlParser().parseConfigurationFile(xmlIS);
        Map<String, Property<?>> properties = conf.getProperties();

        // Override existing configuration within database
        for (Map.Entry<String,Property<?>> featureName : properties.entrySet()) {
            if (existProperty(featureName.getKey())) {
                deleteProperty(featureName.getKey());
            }
            createProperty(featureName.getValue());
        }
        return properties;
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean isEmpty() {
        Set < String > pNames = listPropertyNames();
        return pNames == null || pNames.isEmpty();
    }
    
    /** {@inheritDoc} */
    public String toJson() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getName() + "\"");
        Set<String> myProperties = readAllProperties().keySet();
        sb.append(",\"numberOfProperties\":" + myProperties.size());
        sb.append(",\"properties\":[");
        boolean first = true;
        for (String myProperty : myProperties) {
            if (!first) {
                sb.append(",");
            }
            first = false;
           sb.append("\"" + myProperty + "\"");
        }
        sb.append("]}");
        return sb.toString();
    }
    
    /**
     * Validate property name and existence
     *
     * @param uid
     *      target uid
     */
    protected void assertPropertyExist(String name) {
        Util.assertHasLength(name);
        if (!existProperty(name)) {
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
        Util.assertHasLength(uid);
        if (existProperty(uid)) {
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
    public <T> void updateProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        // Delete
        deleteProperty(prop.getName());
        // Create
        createProperty(prop);
    }
    
    /** {@inheritDoc} */
    @Override
    public void updateProperty(String name, String newValue) {
        // Read from redis, feature not found if no present
        Property<?> p = readProperty(name);
        // Update within Object
        p.setValueFromString(newValue);
        // Serialization and update key, update TTL
        updateProperty(p);
    }
    
    /** {@inheritDoc} */
    @Override
    public void importProperties(Collection<Property<?>> properties) {
        // Do not use target as the delete/create operation will be traced
        if (properties != null) {
            for (Property<?> property : properties) {
                if (existProperty(property.getName())) {
                    deleteProperty(property.getName());
                }
                createProperty(property);
            }
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Property<?> readProperty(String name, Property < ? > defaultValue) {
        try {
            return readProperty(name);
        } catch(PropertyNotFoundException pnf) {
            return defaultValue;
        }
    }
    
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
    
}
