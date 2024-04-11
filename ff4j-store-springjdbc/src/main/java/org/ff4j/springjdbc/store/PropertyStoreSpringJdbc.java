package org.ff4j.springjdbc.store;

/*-
 * #%L
 * ff4j-store-springjdbc
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

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.springjdbc.store.rowmapper.CustomPropertyRowMapper;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.Util;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.SingleColumnRowMapper;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of {@link PropertyStore} with SpringJDBC.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
@Repository
public class PropertyStoreSpringJdbc extends AbstractPropertyStore {

    /** target mapper. */
    private static CustomPropertyRowMapper PMAPPER = new CustomPropertyRowMapper();
    
    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /**
     * Default constructor.
     */
    public PropertyStoreSpringJdbc() {
    }
    
    /**
     * Default constructor.
     */
    public PropertyStoreSpringJdbc(DataSource ds) {
        this.dataSource = ds;
    }

    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return 1 == getJdbcTemplate().
        		queryForObject(getQueryBuilder().existProperty(), Integer.class, name);
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> ap) {
        Util.assertNotNull(ap);
        Util.assertHasLength(ap.getName());
        if (existProperty(ap.getName())) {
            throw new PropertyAlreadyExistException(ap.getName());
        }
        String fixedValues = null;
        if (ap.getFixedValues() != null && ap.getFixedValues().size() > 0) {
            fixedValues = ap.getFixedValues().toString();
            fixedValues = fixedValues.substring(1, fixedValues.length() - 1);
        }
        getJdbcTemplate().update(getQueryBuilder().createProperty(), 
                ap.getName(), ap.getType(), ap.asString(), 
                ap.getDescription(), fixedValues);
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        Util.assertNotNull(name);
        Util.assertHasLength(name);
        try {
            return getJdbcTemplate().
                    queryForObject(getQueryBuilder().getProperty(), PMAPPER, name);
        } catch (EmptyResultDataAccessException ex) {
            throw new PropertyNotFoundException(name);
        }
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        Util.assertHasLength(name);
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        // Check new value validity
        readProperty(name).fromString(newValue);
        getJdbcTemplate().update(getQueryBuilder().updateProperty(), newValue, name);
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
        Util.assertHasLength(name);
        if (!existProperty(name)) {
            throw new PropertyNotFoundException(name);
        }
        getJdbcTemplate().update(getQueryBuilder().deleteProperty(), name);
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> properties = new LinkedHashMap<String, Property<?>>();
        List<Property<?>> listOfProps = getJdbcTemplate().
        		query(getQueryBuilder().getAllProperties(), PMAPPER);
        for(Property<?> p : listOfProps) {
            properties.put(p.getName(),  p);
        }
        return properties;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return new HashSet<String>(getJdbcTemplate().query(
        		getQueryBuilder().getAllPropertiesNames(), new SingleColumnRowMapper<String>()));
    }

    /** {@inheritDoc} */
    public void clear() {
        getJdbcTemplate().update(getQueryBuilder().deleteAllProperties());
    }
    
    /**
     * @param dataSource
     *            the dataSource to set
     */
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Getter accessor for attribute 'jdbcTemplate'.
     * 
     * @return current value of 'jdbcTemplate'
     */
    public JdbcTemplate getJdbcTemplate() {
        if (jdbcTemplate == null) {
            if (dataSource == null) {
                throw new IllegalStateException("ff4j-jdbc: DatabaseStore has not been properly initialized, datasource is null");
            }
            this.jdbcTemplate = new JdbcTemplate(dataSource);
        }
        return jdbcTemplate;
    }

    /**
	 * @return the queryBuilder
	 */
	public JdbcQueryBuilder getQueryBuilder() {
		if (queryBuilder == null) {
			queryBuilder = new JdbcQueryBuilder();
		}
		return queryBuilder;
	}

	/**
	 * @param queryBuilder the queryBuilder to set
	 */
	public void setQueryBuilder(JdbcQueryBuilder queryBuilder) {
		this.queryBuilder = queryBuilder;
	}

	/** {@inheritDoc} */
    @Override
    @Transactional
    public void createSchema() {
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!JdbcUtils.isTableExist(dataSource, qb.getTableNameProperties())) {
            getJdbcTemplate().update(qb.sqlCreateTableProperties());
        }
    }
}
