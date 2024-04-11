package org.ff4j.couchdb.store;

/*-
 * #%L
 * ff4j-store-couchdb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ektorp.CouchDbConnector;
import org.ektorp.DocumentNotFoundException;
import org.ektorp.UpdateConflictException;
import org.ff4j.couchdb.CouchDbConnection;
import org.ff4j.couchdb.CouchDbPropertyView;
import org.ff4j.couchdb.document.CouchDbProperty;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;
import org.lightcouch.CouchDbException;

import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import static org.ff4j.couchdb.CouchDbConstants.DEFAULT_PROPERTY_TYPE;

/**
 * Implementation of the store with REST.
 *
 * @author Curtis White (@drizztguen77)
 */
public class PropertyStoreCouchDb extends AbstractPropertyStore {

    /**
     * Current CouchDB connection.
     */
    private CouchDbConnection couchDbConnection;

    /**
     * Current CouchDB connector.
     */
    private CouchDbConnector couchDbConnector;

    /**
     * Repository class to query couchDB
     */
    private CouchDbPropertyView couchDbPropertyView;

    /**
     * Default constructor
     */
    public PropertyStoreCouchDb() {
    }

    /**
     * Parameterized constructor with database connection.
     *
     * @param couchDbConnection         the database connection to set
     * @param couchDbPropertyView property repository
     */
    public PropertyStoreCouchDb(CouchDbConnection couchDbConnection, CouchDbPropertyView couchDbPropertyView) {
        this.couchDbConnection = couchDbConnection;
        this.couchDbConnector = couchDbConnection.getCouchDbConnector();
        this.couchDbPropertyView = couchDbPropertyView;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        // Do nothing here since CouchDB does not have collection or any other table
        // structure. There is nothing to create
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);

        return null != getProperty(name);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public <T> void createProperty(Property<T> prop) {
        if (prop == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (existProperty(prop.getName())) {
            throw new PropertyAlreadyExistException(prop.getName());
        }

        CouchDbProperty couchDbProperty = new CouchDbProperty();
        couchDbProperty.setType(DEFAULT_PROPERTY_TYPE);
        couchDbProperty.setProperty(prop.toJson());
        createProperty(couchDbProperty);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        CouchDbProperty couchDbProperty = getProperty(name);
        return fromJson(couchDbProperty.getProperty());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void deleteProperty(String name) {
        assertPropertyExist(name);

        CouchDbProperty couchDbProperty = getProperty(name);
        removeProperty(couchDbProperty);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        LinkedHashMap<String, Property<?>> propMap = new LinkedHashMap<>();

        List<CouchDbProperty> properties = getProperties();
        properties.forEach(p -> {
            Property<?> prop = fromJson(p.getProperty());
            if (null != prop) {
                propMap.put(prop.getName(), prop);
            }
        });

        return propMap;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void clear() {
        // Delete all the properties
        List<CouchDbProperty> properties = getProperties();
        properties.forEach(this::removeProperty);
    }

    /**
     * Gets a list of all properties in the database
     *
     * @return list of CouchDB properties
     * @throws CouchDbException If the query failed to execute or the request is invalid.
     */
    public List<CouchDbProperty> getProperties() {
        return this.couchDbPropertyView.getAll();
    }

    /**
     * Get a single property from CouchDB
     *
     * @param name Name of the property to get
     * @return the CouchDB property
     * @throws DocumentNotFoundException If the document is not found in the database.
     */
    public CouchDbProperty getProperty(String name) {
        return couchDbPropertyView.getAll().stream().filter(f -> {
            Property<?> prop = fromJson(f.getProperty());
            return null != prop && prop.getName().equals(name);
        }).findFirst().orElse(null);
    }

    /**
     * Create a single CouchDB property.
     *
     * @param couchDbProperty CouchDB property to create
     * @throws UpdateConflictException If a conflict is detected during the create.
     */
    public void createProperty(CouchDbProperty couchDbProperty) {
        this.couchDbPropertyView.add(couchDbProperty);
    }

    /**
     * Update a single CouchDB property
     *
     * @param couchDbProperty CouchDB property to update
     * @throws UpdateConflictException If a conflict is detected during the update.
     */
    public void updateProperty(CouchDbProperty couchDbProperty) {
        this.couchDbPropertyView.update(couchDbProperty);
    }


    private Property<?> fromJson(String json) {
        return PropertyJsonParser.parseProperty(json);
    }

    /**
     * Update a single CouchDB property
     *
     * @param couchDbProperty CouchDB property to update
     * @throws UpdateConflictException If the document is not found in the database.
     */
    public void removeProperty(CouchDbProperty couchDbProperty) {
        this.couchDbPropertyView.remove(couchDbProperty);
    }

    /**
     * Setter to set the CouchDB connection
     *
     * @param couchDbConnection CouchDB connection
     */
    public void setCouchDbConnector(CouchDbConnection couchDbConnection) {
        this.couchDbConnection = couchDbConnection;
    }

    /**
     * Setter to set the property repository
     *
     * @param couchDbPropertyView Property repository
     */
    public void setCouchDbFeatureRepository(CouchDbPropertyView couchDbPropertyView) {
        this.couchDbPropertyView = couchDbPropertyView;
    }

    /**
     * Getter accessor for attribute 'couchDbConnection'.
     *
     * @return
     *       current value of 'couchDbConnection'
     */
    public CouchDbConnection getCouchDbConnection() {
        return couchDbConnection;
    }

    /**
     * Getter accessor for attribute 'couchDbConnector'.
     *
     * @return
     *       current value of 'couchDbConnector'
     */
    public CouchDbConnector getCouchDbConnector() {
        return couchDbConnector;
    }

    /**
     * Getter accessor for attribute 'couchDbPropertyView'.
     *
     * @return
     *       current value of 'couchDbPropertyView'
     */
    public CouchDbPropertyView getCouchDbPropertyView() {
        return couchDbPropertyView;
    }
}
