package org.ff4j.neo4j.store;

/*
 * #%L
 * ff4j-store-neo4j
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


import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.neo4j.FF4jNeo4jLabels;
import org.ff4j.neo4j.mapper.Neo4jMapper;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import org.neo4j.graphdb.GraphDatabaseService;
import org.neo4j.graphdb.Node;
import org.neo4j.graphdb.Result;
import org.neo4j.graphdb.Transaction;

import static org.ff4j.neo4j.FF4jNeo4jConstants.*;

/**
 *Implementation of {@link PropertyStore} to work with Neo4J.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreNeo4j extends AbstractPropertyStore {
    
    /** Persistent storage. */
    private GraphDatabaseService graphDb;
    
    /**
     * Default to create instances.
     */
    public PropertyStoreNeo4j() {}
    
    /**
     * Initialization of store.
     */
    public PropertyStoreNeo4j(GraphDatabaseService myDb) {
        this.graphDb = myDb;
    }

    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        Map < String, Object > queryParameters = new HashMap<>();
        queryParameters.put("name", name);
        Result result = graphDb.execute(QUERY_CYPHER_EXISTS_PROPERTY,  queryParameters);
        Object count = null;
        if (result.hasNext()) {
            count = result.next().get(QUERY_CYPHER_ALIAS);
        }
        return (null != count) && (((long) count) > 0);
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> ap) {
        Util.assertNotNull(ap);
        Util.assertHasLength(ap.getName());
        if (existProperty(ap.getName())) {
            throw new PropertyAlreadyExistException(ap.getName());
        }
        StringBuilder cypherCreate = new StringBuilder();
        cypherCreate.append("CREATE (p:" + FF4jNeo4jLabels.FF4J_PROPERTY + " { name :'");
        cypherCreate.append(ap.getName());
        cypherCreate.append("', type:'");
        cypherCreate.append(ap.getType());
        cypherCreate.append("', value:'");
        cypherCreate.append(ap.getValue());
        cypherCreate.append("', fixedValues:[");
        if (ap.getFixedValues() != null && !ap.getFixedValues().isEmpty()) {
            boolean first = true;
            for (Object fix : ap.getFixedValues()) {
                if (!first) {
                    cypherCreate.append(",");
                }
                cypherCreate.append("'" + fix.toString() + "'");
                first = false;
            }
        }
        cypherCreate.append("]");
        if (ap.getDescription() != null && ap.getDescription().length() > 0) {
            cypherCreate.append(", description:'");
            cypherCreate.append(ap.getDescription());
            cypherCreate.append("'");
        }
        cypherCreate.append("});");
        Transaction tx = graphDb.beginTx();
        graphDb.execute(cypherCreate.toString());
        tx.success();
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        assertPropertyExist(name);
        Property<?> pro;
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("name", name);
        Result result = graphDb.execute(QUERY_CYPHER_READ_PROPERTY, queryParameters);
        // Property must exist (existProperty executed before) : no JNPE here nor hasNext() tested
        Node node = (Node) result.next().get("p");
        pro = Neo4jMapper.fromNode2Property(node);
        tx.success();
        return pro;
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        assertPropertyExist(name);
        // Check new value validity
        readProperty(name).fromString(newValue);
        Transaction tx = graphDb.beginTx();
        Map<String, Object> queryParameters = new HashMap<>();
        queryParameters.put("name", name);
        queryParameters.put("value", newValue);
        graphDb.execute(QUERY_CYPHER_UPDATE_PROPERTYVALUE, queryParameters);
        tx.success();
    }

    /** {@inheritDoc} */
    public <T> void updateProperty(Property<T> prop) {
        Util.assertNotNull(prop);
        // Delete
        deleteProperty(prop.getName());
        // Create
        createProperty(prop);
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        assertPropertyExist(name);
        Map<String, Object> paramUID = new HashMap<>();
        paramUID.put("name", name);
        Transaction tx = graphDb.beginTx();
        // Delete feature
        graphDb.execute(QUERY_CYPHER_DELETE_PROPERTY, paramUID);
        tx.success();
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> allProperties = new HashMap<>();
        Transaction tx = graphDb.beginTx();
        // Node with relationships
        Result result = graphDb.execute(QUERY_CYPHER_READ_ALLPROPERTIES);
        while (result.hasNext()) {
            Node node = (Node) result.next().get("p");
            Property<?> current = Neo4jMapper.fromNode2Property(node);
            allProperties.put(current.getName(), current);
        }
        tx.success();
        return allProperties;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        Result result = graphDb.execute(QUERY_READ_PROPERTYNAMES);
        Set < String > response = new HashSet<>();
        while (result.hasNext()) {
            response.add((String) result.next().get("NAME"));
        }
        return response;
    }

    /** {@inheritDoc} */
    public void clear() {
        Transaction tx = graphDb.beginTx();
        graphDb.execute(QUERY_CYPHER_DELETE_ALLPROPERTY);
        tx.success();
    }

    /**
     * Getter accessor for attribute 'graphDb'.
     *
     * @return
     *       current value of 'graphDb'
     */
    public GraphDatabaseService getGraphDb() {
        return graphDb;
    }

    /**
     * Setter accessor for attribute 'graphDb'.
     * @param graphDb
     * 		new value for 'graphDb '
     */
    public void setGraphDb(GraphDatabaseService graphDb) {
        this.graphDb = graphDb;
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        // TODO Auto-generated method stub
        
    }

}
