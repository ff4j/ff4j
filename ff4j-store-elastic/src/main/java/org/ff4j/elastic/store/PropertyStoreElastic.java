package org.ff4j.elastic.store;

/*-
 * #%L
 * ff4j-store-elastic
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

import static org.ff4j.elastic.ElasticQueryBuilder.findAllProperties;
import static org.ff4j.elastic.ElasticQueryBuilder.findPropertyByName;
import static org.ff4j.elastic.ElasticQueryHelper.createIndexIfNotExist;
import static org.ff4j.elastic.ElasticQueryHelper.findPropertyTechIdFromName;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.client.JestClient;
import io.searchbox.core.Index;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;

/**
 * Implementation of {@link PropertyStore} to work with Elastic 6+.
 * 
 * <p>Connectivity is based on JestClient
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreElastic extends AbstractPropertyStore {

    /** Logger for the class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(PropertyStoreElastic.class);
   
    /** if no value provide for index use this one. */
    public static String DEFAULT_INDEX_PROPERTIES = "ff4j_properties";
    
    /** Injection of connection to elastic. */
    private JestClient jestClient;
    
    /** Default name of the index in elastic. */
    private String indexProperties = DEFAULT_INDEX_PROPERTIES;

    /**
     * Default constructor.
     */
    public PropertyStoreElastic() {}

    /**
     * Initialization through {@link ElasticConnection}.
     *
     * @param connection
     *            current client to Elasticsearch database
     */
    public PropertyStoreElastic(JestClient jestClient) {
        this(jestClient, DEFAULT_INDEX_PROPERTIES);
    }

    /**
     * Initialization with Connection and initialisation file.
     *
     * @param connection
     * @param xmlFile
     */
    public PropertyStoreElastic(JestClient jestClient, String indexName) {
        this.jestClient    = jestClient;
        this.indexProperties = indexName;
        createSchema();
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        LOGGER.info("Creating index {} (if needed)", indexProperties);
        createIndexIfNotExist(jestClient, indexProperties);
    }
    
	 /** {@inheritDoc} */
    @Override
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        try {
            Search       query  = ElasticQueryBuilder.findPropertyByName(indexProperties, name);
            SearchResult result = jestClient.execute(query);
            if (!result.isSucceeded()) {
                throw new IllegalStateException(
                        "Error in query '" + result.getErrorMessage() + "'") ;
            }
            return (result != null) && 
                   (result.getHits(Map.class) != null) && 
                   !result.getHits(Map.class).isEmpty();
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot check property existence for '" + name + "'", e);
        }
    }

	/** {@inheritDoc} */
	@Override
	public <T> void createProperty(Property<T> property) {
	    assertPropertyNotNull(property);
        assertPropertyNotExist(property.getName());
        try {
            Index creationQuery = ElasticQueryBuilder.createProperty(indexProperties, property);
            jestClient.execute(creationQuery);
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot create property '" + property.getName() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	public Property<?> readProperty(String name) {
		assertPropertyExist(name);
		 try {
	            Search search = findPropertyByName(indexProperties, name);
	            return jestClient.execute(search).getFirstHit(Property.class).source;
	     } catch (IOException e) {
	            throw new PropertyAccessException("Cannot read feature '" + name + "'", e);
	     }
	}

	/** {@inheritDoc} */
	@Override
	public void deleteProperty(String name) {
		assertPropertyExist(name);
		
        try {
            String techid = findPropertyTechIdFromName(jestClient, indexProperties, name);
            jestClient.execute(ElasticQueryBuilder.deleteProperty(indexProperties, techid, name));
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot delete property '" + name + "'", e);
        }
	}

	/** {@inheritDoc} */
	@SuppressWarnings("rawtypes")
    @Override
	public Map<String, Property<?>> readAllProperties() {
	    Map<String, Property<?>> mapOfProperties = new HashMap<String, Property<?>>();
        try {
            Search query = findAllProperties(indexProperties);
            for (Hit<Property, Void> property : jestClient.execute(query).getHits(Property.class)) {
                mapOfProperties.put(property.source.getName(), property.source);
            }
            return mapOfProperties;
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot read  all features", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	public Set<String> listPropertyNames() {
		return readAllProperties().keySet();
	}

    /** {@inheritDoc} */
    @Override
    public void clear() {
        try {
            jestClient.execute(ElasticQueryBuilder.deleteAllProperties(indexProperties));
        } catch (IOException e) {
            throw new PropertyAccessException("Cannot remove all properties", e);
        }
    }

    /**
     * Getter accessor for attribute 'indexProperties'.
     *
     * @return
     *       current value of 'indexProperties'
     */
    public String getIndexProperties() {
        return indexProperties;
    }

    /**
     * Setter accessor for attribute 'indexProperties'.
     * @param indexProperties
     * 		new value for 'indexProperties '
     */
    public void setIndexProperties(String indexProperties) {
        this.indexProperties = indexProperties;
    }

}
