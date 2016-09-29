package org.ff4j.property.store;

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
import static org.ff4j.store.JdbcStoreConstants.COL_PROPERTY_ID;
import static org.ff4j.utils.JdbcUtils.buildStatement;
import static org.ff4j.utils.JdbcUtils.closeConnection;
import static org.ff4j.utils.JdbcUtils.closeResultSet;
import static org.ff4j.utils.JdbcUtils.closeStatement;
import static org.ff4j.utils.JdbcUtils.executeUpdate;
import static org.ff4j.utils.JdbcUtils.isTableExist;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.Util;

/**
 * Access information related to properties within database.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcPropertyStore extends AbstractPropertyStore {

    /** Access to storage. */
    private DataSource dataSource;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /** Mapper. */
    private JdbcPropertyMapper JDBC_MAPPER = new JdbcPropertyMapper();

    /** Default Constructor. */
    public JdbcPropertyStore() {}
    
    /**
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcPropertyStore(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /**s
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcPropertyStore(DataSource jdbcDS, String xmlConfFile) {
        this(jdbcDS);
        importPropertiesFromXmlFile(xmlConfFile);
    }
    

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!isTableExist(ds, qb.getTableNameProperties())) {
            executeUpdate(ds, qb.sqlCreateTableProperties());
        }
    }
     
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        PreparedStatement  ps = null;
        ResultSet          rs = null;
        Connection         sqlConn = null;
        try {
           sqlConn = getDataSource().getConnection();
           ps = buildStatement(sqlConn, getQueryBuilder().existProperty(), name);
           rs = ps.executeQuery();
           rs.next();
           return 1 == rs.getInt(1);
        } catch (SQLException sqlEX) {
           throw new PropertyAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> ap) {
        Util.assertNotNull(ap);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {
            sqlConn = getDataSource().getConnection();
            if (existProperty(ap.getName())) {
                throw new PropertyAlreadyExistException(ap.getName());
            }
            ps = sqlConn.prepareStatement(getQueryBuilder().createProperty());
            ps.setString(1, ap.getName());
            ps.setString(2, ap.getType());
            ps.setString(3, ap.asString());
            ps.setString(4, ap.getDescription());
            if (ap.getFixedValues() != null && !ap.getFixedValues().isEmpty()) {
                String fixedValues = ap.getFixedValues().toString();
                ps.setString(5, fixedValues.substring(1, fixedValues.length() - 1));
            } else {
                ps.setString(5, null);
            }
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot update properties database, SQL ERROR", sqlEX);
        } finally {
            // Connection is closed alse here within clos statement
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        Util.assertHasLength(name);
        Connection   sqlConn = null;
        PreparedStatement ps = null;
        ResultSet         rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            if (!existProperty(name)) {
                throw new PropertyNotFoundException(name);
            }
            // Returns features
            ps = buildStatement(sqlConn, getQueryBuilder().getProperty(), name);
            rs = ps.executeQuery();
            rs.next();
            return JDBC_MAPPER.map(rs);
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot check property existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public void updateProperty(String name, String newValue) {
        Util.assertHasLength(name);
        Connection   sqlConn = null;
        PreparedStatement ps = null;
        try {
            sqlConn = getDataSource().getConnection();
            // Check existence
            Property<?> ab = readProperty(name);
            // Check new value validity
            ab.fromString(newValue);
            ps = buildStatement(sqlConn, getQueryBuilder().updateProperty(), newValue, name);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot update property database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public <T> void updateProperty(Property<T> prop) {
        if (prop == null || prop.getName() == null) {
            throw new IllegalArgumentException("Cannot update property, please provide property name");
        }
        deleteProperty(prop.getName());
        createProperty(prop);
    }
   
    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        Util.assertHasLength(name);
        Connection   sqlConn = null;
        PreparedStatement ps = null;
        try {
            sqlConn = getDataSource().getConnection();
            if (!existProperty(name)) {
                throw new PropertyNotFoundException(name);
            }
            ps = buildStatement(sqlConn, getQueryBuilder().deleteProperty(), name);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot delete property database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map<String, Property<?>> properties = new LinkedHashMap<String, Property<?>>();
        Connection   sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = buildStatement(sqlConn, getQueryBuilder().getAllProperties());
            rs = ps.executeQuery();
            while (rs.next()) {
                Property<?> ap = JDBC_MAPPER.map(rs);
                properties.put(ap.getName(),ap);
            }
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot read properties within database, SQL ERROR", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return properties;
    }
    
    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        Set < String > propertyNames = new HashSet<String>();
        PreparedStatement ps = null;
        Connection   sqlConn = null;
        ResultSet rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = buildStatement(sqlConn, getQueryBuilder().getAllPropertiesNames());
            rs = ps.executeQuery();
            while (rs.next()) {
               propertyNames.add(rs.getString(COL_PROPERTY_ID));
            }
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot read properties within database, SQL ERROR", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
        return propertyNames;
    }

    /** {@inheritDoc} */
    public void clear() {
        PreparedStatement ps = null;
        Connection   sqlConn = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = buildStatement(sqlConn, getQueryBuilder().deleteAllProperties());
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new PropertyAccessException("Cannot clear properties table, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /**
     * Getter accessor for attribute 'dataSource'.
     *
     * @return
     *       current value of 'dataSource'
     */
    public DataSource getDataSource() {
        return dataSource;
    }

    /**
     * Setter accessor for attribute 'dataSource'.
     * @param dataSource
     * 		new value for 'dataSource '
     */
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
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
    
}
