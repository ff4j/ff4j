package org.ff4j.store;

import static org.ff4j.store.JdbcQueryBuilder.COL_FEAT_GROUPNAME;
import static org.ff4j.store.JdbcQueryBuilder.COL_ROLE_FEATID;
import static org.ff4j.store.JdbcQueryBuilder.COL_ROLE_ROLENAME;
import static org.ff4j.utils.JdbcUtils.buildStatement;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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

import static org.ff4j.utils.JdbcUtils.closeConnection;
import static org.ff4j.utils.JdbcUtils.closeResultSet;
import static org.ff4j.utils.JdbcUtils.closeStatement;
import static org.ff4j.utils.JdbcUtils.rollback;
import static org.ff4j.utils.Util.assertHasLength;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.JdbcPropertyMapper;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcFeatureStore extends AbstractFeatureStore {

	/** Error message 1. */
    public static final String CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE = 
    		"Cannot check feature existence, error related to database";
    
    /** Error message 2. */
    public static final String CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR = 
    		"Cannot update features database, SQL ERROR";
    
    /** Access to storage. */
    private DataSource dataSource;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /** Mapper. */
    private JdbcPropertyMapper JDBC_PROPERTY_MAPPER = new JdbcPropertyMapper();

    /** Mapper. */
    private JdbcFeatureMapper JDBC_FEATURE_MAPPER = new JdbcFeatureMapper();

    /** Default Constructor. */
    public JdbcFeatureStore() {}

    /**
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcFeatureStore(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /**
     * Constructor from DataSource.
     * 
     * @param jdbcDS
     *            native jdbc datasource
     */
    public JdbcFeatureStore(DataSource jdbcDS, String xmlConfFile) {
        this(jdbcDS);
        importFeaturesFromXmlFile(xmlConfFile);
    }

    /** {@inheritDoc} */
    public void enable(String uid) {
    	assertFeatureExist(uid);
        update(getQueryBuilder().enableFeature(), uid);
    }

    /** {@inheritDoc} */
    public void disable(String uid) {
    	assertFeatureExist(uid);
        update(getQueryBuilder().disableFeature(), uid);
    }

    /** {@inheritDoc} */
    public boolean exist(String uid) {
    	assertHasLength(uid);
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = JdbcUtils.buildStatement(sqlConn, getQueryBuilder().existFeature(), uid);
            rs = ps.executeQuery();
            rs.next();
            return 1 == rs.getInt(1);
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */    
    @SuppressWarnings("resource")
	public Feature read(String uid) {
    	assertFeatureExist(uid);
        Connection          sqlConn = null;
        PreparedStatement   ps = null;
        ResultSet           rs = null;
        try {
            sqlConn = getDataSource().getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeature());
            ps.setString(1, uid);
            rs = ps.executeQuery();
            Feature f = null;
            if (rs.next()) {
                f = JDBC_FEATURE_MAPPER.mapFeature(rs);
            } else {
                throw new FeatureNotFoundException(uid);
            }

            // Enrich to get roles 2nd request
            ps = sqlConn.prepareStatement(getQueryBuilder().getRoles());
            ps.setString(1, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
                f.getPermissions().add(rs.getString("ROLE_NAME"));
            }
            
            // Enrich with properties 3d request to get custom properties by uid
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureProperties());
            ps.setString(1, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
                Property<?> ap = JDBC_PROPERTY_MAPPER.map(rs);
                f.getCustomProperties().put(ap.getName(), ap);
            }
            return f;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public void create(Feature fp) {
    	assertFeatureNotNull(fp);
    	Connection sqlConn = null;
        PreparedStatement ps = null;
        try {

            // Create connection
            sqlConn = getDataSource().getConnection();
            if (exist(fp.getUid())) {
                throw new FeatureAlreadyExistException(fp.getUid());
            }
            
            // Begin TX
            sqlConn.setAutoCommit(false);

            // Create feature
            ps = sqlConn.prepareStatement(getQueryBuilder().createFeature());
            ps.setString(1, fp.getUid());
            ps.setInt(2, fp.isEnable() ? 1 : 0);
            ps.setString(3, fp.getDescription());
            String strategyColumn = null;
            String expressionColumn = null;
            if (fp.getFlippingStrategy() != null) {
                strategyColumn   = fp.getFlippingStrategy().getClass().getCanonicalName();
                expressionColumn = MappingUtil.fromMap(fp.getFlippingStrategy().getInitParams());
            }
            ps.setString(4, strategyColumn);
            ps.setString(5, expressionColumn);
            ps.setString(6, fp.getGroup());
            ps.executeUpdate();

            // Create roles
            if (fp.getPermissions() != null) {
                for (String role : fp.getPermissions()) {
                    ps = sqlConn.prepareStatement(getQueryBuilder().addRoleToFeature());
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
                    ps.executeUpdate();
                }
            }
            
            // Create customproperties
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                for (Property<?> pp : fp.getCustomProperties().values()) {
                    ps = createCustomProperty(sqlConn, fp.getUid(), pp);
                }
            }

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("resource")
	public void delete(String uid) {
    	assertFeatureExist(uid);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {
            // Create connection
            sqlConn = getDataSource().getConnection();
            sqlConn.setAutoCommit(false);
            Feature fp = read(uid);
            
            // Delete Properties
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                for (String property : fp.getCustomProperties().keySet()) {
                    ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeatureProperty());
                    ps.setString(1, property);
                    ps.setString(2, fp.getUid());
                    ps.executeUpdate();
                }
            }

            // Delete Roles
            if (fp.getPermissions() != null) {
                for (String role : fp.getPermissions()) {
                    ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeatureRole());
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
                    ps.executeUpdate();
                }
            }

            // Delete Feature
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeature());
            ps.setString(1, fp.getUid());
            ps.executeUpdate();

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
  
    /** {@inheritDoc} */ 
    public void grantRoleOnFeature(String uid, String roleName) {
    	assertFeatureExist(uid);
        assertHasLength(roleName);
        update(getQueryBuilder().addRoleToFeature(), uid, roleName);
    }

    /** {@inheritDoc} */
    public void removeRoleFromFeature(String uid, String roleName) {
    	assertFeatureExist(uid);
        assertHasLength(roleName);
        update(getQueryBuilder().deleteFeatureRole(), uid, roleName);
    }
    
    /** {@inheritDoc} */
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            
        	// Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getAllFeatures());
            rs = ps.executeQuery();
            while (rs.next()) {
                Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(
            		getQueryBuilder().getAllRoles()).executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
            }
            return mapFP;

        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public Set<String> readAllGroups() {
        Set<String> setOFGroup = new HashSet<String>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getAllGroups());
            rs = ps.executeQuery();
            while (rs.next()) {
                String groupName = rs.getString(COL_FEAT_GROUPNAME);
                if (groupName != null && !"".equals(groupName)) {
                    setOFGroup.add(groupName);
                }
            }
            return setOFGroup;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot list groups, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public void update(Feature fp) {
    	assertFeatureNotNull(fp);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        
        try {
            sqlConn = dataSource.getConnection();
            Feature fpExist = read(fp.getUid());
            String enable = "0";
            if (fp.isEnable()) {
                enable = "1";
            }
            String fStrategy = null;
            String fExpression = null;
            if (fp.getFlippingStrategy() != null) {
                fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
                fExpression = MappingUtil.fromMap(fp.getFlippingStrategy().getInitParams());
            }
            update(getQueryBuilder().updateFeature(), 
            		enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(), fp.getUid());
    
            // ROLES
            
            // To be deleted (not in new value but was at first)
            Set<String> toBeDeleted = new HashSet<String>();
            toBeDeleted.addAll(fpExist.getPermissions());
            toBeDeleted.removeAll(fp.getPermissions());
            for (String roleToBeDelete : toBeDeleted) {
                removeRoleFromFeature(fpExist.getUid(), roleToBeDelete);
            }
    
            // To be created : in second but not in first
            Set<String> toBeAdded = new HashSet<String>();
            toBeAdded.addAll(fp.getPermissions());
            toBeAdded.removeAll(fpExist.getPermissions());
            for (String addee : toBeAdded) {
                grantRoleOnFeature(fpExist.getUid(), addee);
            }
            
            // REMOVE EXISTING CUSTOM PROPERTIES
            if (fpExist.getCustomProperties() != null && !fpExist.getCustomProperties().isEmpty()) {
                ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllFeatureCustomProperties());
                ps.setString(1, fpExist.getUid());
                ps.executeUpdate();
            }
            
            // CREATE PROPERTIES
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                createCustomProperties(fp.getUid(), fp.getCustomProperties().values());
            } 
            } catch (SQLException sqlEX) {
                throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
            } finally {
                closeStatement(ps);
                closeConnection(sqlConn);
            }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("resource")
	public void clear() {
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {
            
            sqlConn = dataSource.getConnection();
            
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllCustomProperties());
            ps.executeUpdate();
            
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllRoles());
            ps.executeUpdate();
            
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllFeatures());
            ps.executeUpdate();
            
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /**
     * Ease creation of properties in Database.
     * 
     * @param uid
     *      target unique identifier
     * @param props
     *      target properties.
     */
    public void createCustomProperties(String uid, Collection <Property<?> > props) {
        Util.assertNotNull(uid);
        if (props == null || props.isEmpty()) return;
       
        Connection sqlConn = null;
        PreparedStatement ps = null;
        
        try {
            sqlConn = dataSource.getConnection();
            
            // Begin TX
            sqlConn.setAutoCommit(false);
            
            // Queries
            for (Property<?> pp : props) {
                ps = createCustomProperty(sqlConn, uid, pp);
            }
            
            // End TX
            sqlConn.commit();
            
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }
    
    /**
     * Create SQL statement to create property.
     *
     * @param sqlConn
     * 		current sql connection
     * @param featureId
     * 		current unique feature identifier
     * @param pp
     * 		pojo property
     * @return
     * 		statement sql to be executed
     * @throws SQLException
     * 		error during sql operation
     */
    private PreparedStatement createCustomProperty(Connection sqlConn, String featureId, Property<?> pp)
    throws SQLException {
        PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().createFeatureProperty());
        ps.setString(1, pp.getName());
        ps.setString(2, pp.getType());
        ps.setString(3, pp.asString());
        ps.setString(4, pp.getDescription());
        if (pp.getFixedValues() != null && !pp.getFixedValues().isEmpty()) {
            String fixedValues = pp.getFixedValues().toString();
            ps.setString(5, fixedValues.substring(1, fixedValues.length() - 1));
        } else {
            ps.setString(5, null);
        }
        ps.setString(6, featureId);
        ps.executeUpdate();
        return ps;
    }

    /** {@inheritDoc} */
    public boolean existGroup(String groupName) {
    	assertHasLength(groupName);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().existGroup());
            ps.setString(1, groupName);
            rs = ps.executeQuery();
            rs.next(); 
            return rs.getInt(1) > 0;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public void enableGroup(String groupName) {
    	assertGroupExist(groupName);
        update(getQueryBuilder().enableGroup(), groupName);
    }

    /** {@inheritDoc} */
    public void disableGroup(String groupName) {
    	assertGroupExist(groupName);
        update(getQueryBuilder().disableGroup(), groupName);
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readGroup(String groupName) {
    	assertGroupExist(groupName);
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            if (!existGroup(groupName)) {
                throw new GroupNotFoundException(groupName);
            }
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureOfGroup());
            ps.setString(1, groupName);
            rs = ps.executeQuery();
            while (rs.next()) {
                Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(
            		getQueryBuilder().getAllRoles()).executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                // only feature in the group must be processed
                if (mapFP.containsKey(uid)) {
                    mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
                }
            }
            return mapFP;

        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    public void addToGroup(String uid, String groupName) {
    	assertFeatureExist(uid);
        assertHasLength(groupName);
        update(getQueryBuilder().addFeatureToGroup(), groupName, uid);
    }

    /** {@inheritDoc} */
    public void removeFromGroup(String uid, String groupName) {
    	assertFeatureExist(uid);
        assertGroupExist(groupName);
        Feature feat = read(uid);
        if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
            throw new IllegalArgumentException("'" + uid + "' is not in group '" + groupName + "'");
        }
        update(getQueryBuilder().addFeatureToGroup(), "", uid);
    }
    
    /**
     * Utility method to perform UPDATE and DELETE operations.
     * 
     * @param query
     *            target query
     * @param params
     *            sql query params
     */
    public void update(String query, String... params) {
        Connection sqlConnection = null;
        PreparedStatement ps = null;
        try {
            sqlConnection = dataSource.getConnection();
            ps = buildStatement(sqlConnection, query, params);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConnection);
        }
    }

    /**
     * Getter accessor for attribute 'dataSource'.
     * 
     * @return current value of 'dataSource'
     */
    public DataSource getDataSource() {
    	if (dataSource == null) {
    		throw new IllegalStateException("DataSource has not been initialized");
    	}
        return dataSource;
    }

    /**
     * Setter accessor for attribute 'dataSource'.
     * 
     * @param dataSource
     *            new value for 'dataSource '
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
