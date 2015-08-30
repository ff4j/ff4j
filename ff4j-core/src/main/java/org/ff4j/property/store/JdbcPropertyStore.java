package org.ff4j.property.store;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.LinkedHashMap;
import java.util.Map;

import javax.sql.DataSource;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.AbstractProperty;
import org.ff4j.store.JdbcStoreConstants;
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
 * Access information related to properties within database.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcPropertyStore extends AbstractPropertyStore implements JdbcStoreConstants {

    /** Access to storage. */
    private DataSource dataSource;
    
    /** Mapper. */
    private JdbcPropertyMapper JDBC_MAPPER = new JdbcPropertyMapper();

    /**
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcPropertyStore(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /**
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
    public boolean exist(String name) {
        Util.assertHasLength(name);
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = buildStatement(SQL_PROPERTY_EXIST, name);
            rs = ps.executeQuery();
            if (rs.next()) {
                return 1 == rs.getInt(1);
            }
            return false;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public <T> void create(AbstractProperty<T> ap) {
        if (ap == null) {
            throw new IllegalArgumentException("Property cannot be null nor empty");
        }
        if (exist(ap.getName())) {
            throw new PropertyAlreadyExistException(ap.getName());
        }
        PreparedStatement ps = null;
        try {
            Connection sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(SQL_PROPERTY_CREATE);
            ps.setString(1, ap.getName());
            ps.setString(2, ap.getType());
            ps.setString(3, ap.asString());
            ps.setString(4, ap.getDescription());
            if (ap.getFixedValues() != null && ap.getFixedValues().size() > 0) {
                String fixedValues = ap.getFixedValues().toString();
                ps.setString(5, fixedValues.substring(1, fixedValues.length() - 1));
            } else {
                ps.setString(5, null);
            }
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot update properties database, SQL ERROR", sqlEX);
        } finally {
            // Connection is closed alse here within clos statement
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public AbstractProperty<?> read(String name) {
        PreparedStatement ps = null;
        ResultSet rs = null;
        Util.assertHasLength(name);
        
        try {
            // Returns features
            ps = buildStatement(SQL_PROPERTY_READ, name);
            rs = ps.executeQuery();
            if (rs.next()) {
                return JDBC_MAPPER.map(rs);
            } else {
                throw new PropertyNotFoundException(name);
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check property existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(String name, String newValue) {
        Util.assertHasLength(name);
        if (!exist(name)) {
            throw new PropertyNotFoundException(name);
        }
        // Update
        AbstractProperty<?> current = read(name);
        current.setValueFromString(newValue);
        //
        PreparedStatement ps = null;
        try {
            ps = buildStatement(SQL_PROPERTY_UPDATE, newValue, name);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot update property database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public <T> void update(AbstractProperty<T> prop) {
        if (prop == null || prop.getName() == null) {
            throw new IllegalArgumentException("Cannot update property, please provide property name");
        }
        // Delete
        delete(prop.getName());
        // Create
        create(prop);
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String name) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Property identifier (param#0) cannot be null nor empty");
        }
        if (!exist(name)) {
            throw new PropertyNotFoundException(name);
        }
        PreparedStatement ps = null;
        try {
            ps = buildStatement(SQL_PROPERTY_DELETE,name);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot delete property database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, AbstractProperty<?>> readAllProperties() {
        Map<String, AbstractProperty<?>> properties = new LinkedHashMap<String, AbstractProperty<?>>();
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = buildStatement(SQL_PROPERTY_READ);
            rs = ps.executeQuery();
            while (rs.next()) {
                AbstractProperty<?> ap = JDBC_MAPPER.map(rs);
                properties.put(ap.getName(),ap);
            }
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot read properties within database, SQL ERROR", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
        return properties;
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
     * Build {@link PreparedStatement} from parameters
     * 
     * @param query
     *            query template
     * @param params
     *            current parameters
     * @return working {@link PreparedStatement}
     * @throws SQLException
     *             sql error when working with statement
     */
    public PreparedStatement buildStatement(String query, String... params) throws SQLException {
        Connection sqlConn = getDataSource().getConnection();
        PreparedStatement ps = sqlConn.prepareStatement(query);
        if (params != null && params.length > 0) {
            for (int i = 0; i < params.length; i++) {
                ps.setString(i + 1, params[i]);
            }
        }
        return ps;
    }
    
    /**
     * Close resultset.
     * 
     * @param rs
     *            target resultset
     */
    private void closeResultSet(ResultSet rs) {
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing resultset", e);
        }
    }

    /**
     * Utility method to close statement properly.
     * 
     * @param ps
     * 
     */
    private void closeStatement(PreparedStatement ps) {
        try {
            if (ps != null && !ps.isClosed()) {
                 if (ps.getConnection() != null && !ps.getConnection().isClosed()) {
                     ps.getConnection().close();
                 }
                 ps.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing statement", e);
        }
    }

}
