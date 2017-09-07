package org.ff4j.elastic.store;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;

import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;

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

	/** Injection of connection to elastic. */
	private ElasticConnection connection;

	/** Connection to store Elastic. */
	private ElasticQueryBuilder builder;

	/**
	 * Initialization through {@link ElasticConnection}.
	 *
	 * @param connection
	 *            current client to Elasticsearch database
	 */
	public PropertyStoreElastic(ElasticConnection connection) {
		this.connection = connection;
	}

	public PropertyStoreElastic(ElasticConnection connection, String xmlFile) {
		this(connection);
		importPropertiesFromXmlFile(xmlFile);
	}

	/** {@inheritDoc} */
	@Override
	public boolean existProperty(String name) {
		Util.assertHasLength(name);
		SearchResult result = getConnection().search(getBuilder().queryPropertyByName(name), true);
		return (result.getTotal() != null) && (result.getTotal() >= 1);
	}

	/** {@inheritDoc} */
	@Override
	public <T> void createProperty(Property<T> property) {
		assertPropertyNotNull(property);
		assertPropertyNotExist(property.getName());
		getConnection().execute(getBuilder().queryCreateProperty(property));
	}

	/** {@inheritDoc} */
	@Override
	public Property<?> readProperty(String name) {
		assertPropertyExist(name);
		SearchResult result = getConnection().search(getBuilder().queryPropertyByName(name), true);
		return result.getFirstHit(Property.class).source;
	}

	/** {@inheritDoc} */
	@Override
	public void deleteProperty(String name) {
		assertPropertyExist(name);
		getConnection().execute(getBuilder().queryDeletePropertyByName(name));
	}

	/** {@inheritDoc} */
	@SuppressWarnings("rawtypes")
    @Override
	public Map<String, Property<?>> readAllProperties() {
		SearchResult search = getConnection().search(getBuilder().queryReadAllProperties(), true);
		Map<String, Property<?>> mapOfProperties = new HashMap<String, Property<?>>();
		if (null != search && search.isSucceeded()) {
			Integer total = search.getTotal();
			SearchResult searchAllResult = getConnection().search(getBuilder().queryReadAllProperties(total), true);
			if (null != searchAllResult && searchAllResult.isSucceeded()) {
				for (Hit<Property, Void> property : searchAllResult.getHits(Property.class)) {
					mapOfProperties.put(property.source.getName(), property.source);
				}
			}
		}
		return mapOfProperties;
	}

	/** {@inheritDoc} */
	@Override
	public Set<String> listPropertyNames() {
		return readAllProperties().keySet();
	}

	/** {@inheritDoc} */
	@Override
	public void clear() {
		getConnection().execute(getBuilder().queryClear());

	}

	/** {@inheritDoc} */
	@Override
	public void createSchema() {
		getConnection().execute(getBuilder().queryFlushIndex());
	}

	/**
	 * Getter accessor for attribute 'connection'.
	 *
	 * @return current value of 'connection'
	 */
	public ElasticConnection getConnection() {
		return connection;
	}

	/**
	 * Setter accessor for attribute 'connection'.
	 * 
	 * @param connection
	 *            new value for 'connection '
	 */
	public void setConnection(ElasticConnection connection) {
		this.connection = connection;
	}

	/**
	 * Getter accessor for attribute 'builder'.
	 *
	 * @return current value of 'builder'
	 */
	public ElasticQueryBuilder getBuilder() {
		if (builder == null) {
			builder = new ElasticQueryBuilder(this.connection);
		}
		return builder;
	}
}
