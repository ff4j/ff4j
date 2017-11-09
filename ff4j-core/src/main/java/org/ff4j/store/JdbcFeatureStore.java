package org.ff4j.store;

import static org.ff4j.store.JdbcStoreConstants.COL_FEAT_GROUPNAME;
import static org.ff4j.store.JdbcStoreConstants.COL_ROLE_FEATID;
import static org.ff4j.store.JdbcStoreConstants.COL_ROLE_ROLENAME;
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
import static org.ff4j.utils.JdbcUtils.executeUpdate;
import static org.ff4j.utils.JdbcUtils.isTableExist;
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
    @Override
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!isTableExist(ds, qb.getTableNameFeatures())) {
            executeUpdate(ds, qb.sqlCreateTableFeatures());
        }
        if (!isTableExist(ds, qb.getTableNameCustomProperties())) {
            executeUpdate(ds, qb.sqlCreateTableCustomProperties());
        }
        if (!isTableExist(ds, qb.getTableNameRoles())) {
            executeUpdate(ds, qb.sqlCreateTableRoles());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
    	assertFeatureExist(uid);
        update(getQueryBuilder().enableFeature(), uid);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
    	assertFeatureExist(uid);
        update(getQueryBuilder().disableFeature(), uid);
    }

    /** {@inheritDoc} */
    @Override
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
   @Override
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
            // Existence is tested before
            rs.next();
            Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Enrich to get roles 2nd request
            ps = sqlConn.prepareStatement(getQueryBuilder().getRoles());
            ps.setString(1, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
                f.getPermissions().add(rs.getString("ROLE_NAME"));
            }
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Enrich with properties 3d request to get custom properties by uid
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureProperties());
            ps.setString(1, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
               f.addProperty(JDBC_PROPERTY_MAPPER.map(rs));
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
    @Override
    public void create(Feature fp) {
    	assertFeatureNotNull(fp);
    	Connection sqlConn = null;
        PreparedStatement ps = null;
        Boolean previousAutoCommit = null;
        try {

            // Create connection
            sqlConn = getDataSource().getConnection();
            if (exist(fp.getUid())) {
                throw new FeatureAlreadyExistException(fp.getUid());
            }

            // Begin TX
            previousAutoCommit = sqlConn.getAutoCommit();
            sqlConn.setAutoCommit(false);

            // Create feature
            ps = sqlConn.prepareStatement(getQueryBuilder().createFeature());
            ps.setString(1, fp.getUid());
            ps.setInt(2, fp.isEnable() ? 1 : 0);
            ps.setString(3, fp.getDescription());
            String strategyColumn = null;
            String expressionColumn = null;
            if (fp.getFlippingStrategy() != null) {
                strategyColumn   = fp.getFlippingStrategy().getClass().getName();
                expressionColumn = MappingUtil.fromMap(fp.getFlippingStrategy().getInitParams());
            }
            ps.setString(4, strategyColumn);
            ps.setString(5, expressionColumn);
            ps.setString(6, fp.getGroup());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

            // Create roles
            for (String role : fp.getPermissions()) {
                ps = sqlConn.prepareStatement(getQueryBuilder().addRoleToFeature());
                ps.setString(1, fp.getUid());
                ps.setString(2, role);
                ps.executeUpdate();
                closeStatement(ps);
                ps = null;
            }

            // Create customproperties
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                for (Property<?> pp : fp.getCustomProperties().values()) {
                    ps = createCustomProperty(sqlConn, fp.getUid(), pp);
                    closeStatement(ps);
                    ps = null;
                }
            }

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn, previousAutoCommit);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
    	assertFeatureExist(uid);
        Connection sqlConn = null;
        PreparedStatement ps = null;
        Boolean previousAutoCommit = null;
        try {
            // Create connection
            sqlConn = getDataSource().getConnection();
            previousAutoCommit = sqlConn.getAutoCommit();
            sqlConn.setAutoCommit(false);
            Feature fp = read(uid);

            // Delete Properties
            if (fp.getCustomProperties() != null) {
                for (String property : fp.getCustomProperties().keySet()) {
                    ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeatureProperty());
                    ps.setString(1, property);
                    ps.setString(2, fp.getUid());
                    ps.executeUpdate();
                    closeStatement(ps);
                    ps = null;
                }
            }

            // Delete Roles
            if (fp.getPermissions() != null) {
                for (String role : fp.getPermissions()) {
                    ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeatureRole());
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
                    ps.executeUpdate();
                    closeStatement(ps);
                    ps = null;
                }
            }

            // Delete Feature
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteFeature());
            ps.setString(1, fp.getUid());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn, previousAutoCommit);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
    	assertFeatureExist(uid);
        assertHasLength(roleName);
        update(getQueryBuilder().addRoleToFeature(), uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
    	assertFeatureExist(uid);
        assertHasLength(roleName);
        update(getQueryBuilder().deleteFeatureRole(), uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
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
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Returns Roles
            ps = sqlConn.prepareStatement(getQueryBuilder().getAllRoles());
            rs = ps.executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
            }
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Read custom properties for each feature
            for (Feature f : mapFP.values()) {
                ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureProperties());
                ps.setString(1, f.getUid());
                rs = ps.executeQuery();
                while (rs.next()) {
                    f.addProperty(JDBC_PROPERTY_MAPPER.map(rs));
                }
                closeResultSet(rs);
                rs = null;
                closeStatement(ps);
                ps = null;
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
    @Override
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
                if (Util.hasLength(groupName)) {
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
    @Override
    public void update(Feature fp) {
    	assertFeatureNotNull(fp);
        Connection sqlConn = null;
        PreparedStatement ps = null;

        try {
            sqlConn = dataSource.getConnection();
            Feature fpExist = read(fp.getUid());
            int enable = 0;
            if (fp.isEnable()) {
                enable = 1;
            }
            String fStrategy = null;
            String fExpression = null;
            if (fp.getFlippingStrategy() != null) {
                fStrategy = fp.getFlippingStrategy().getClass().getName();
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
            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllFeatureCustomProperties());
            ps.setString(1, fpExist.getUid());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

            // CREATE CUSTOM PROPERTIES
            for (Property<?> property : fp.getCustomProperties().values()) {
                ps = createCustomProperty(sqlConn, fp.getUid(), property);
                closeStatement(ps);
                ps = null;
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {

            sqlConn = dataSource.getConnection();

            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllCustomProperties());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllRoles());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

            ps = sqlConn.prepareStatement(getQueryBuilder().deleteAllFeatures());
            ps.executeUpdate();
            closeStatement(ps);
            ps = null;

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
        if (props == null) return;

        Connection sqlConn = null;
        PreparedStatement ps = null;
        Boolean previousAutoCommit = null;

        try {
            sqlConn = dataSource.getConnection();

            // Begin TX
            previousAutoCommit = sqlConn.getAutoCommit();
            sqlConn.setAutoCommit(false);

            // Queries
            for (Property<?> pp : props) {
                ps = createCustomProperty(sqlConn, uid, pp);
                closeStatement(ps);
                ps = null;
            }

            // End TX
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        } finally {
            closeStatement(ps);
            closeConnection(sqlConn, previousAutoCommit);
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
    @Override
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
    @Override
    public void enableGroup(String groupName) {
    	assertGroupExist(groupName);
        update(getQueryBuilder().enableGroup(), groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
    	assertGroupExist(groupName);
        update(getQueryBuilder().disableGroup(), groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
    	assertGroupExist(groupName);
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        Connection sqlConn = null;
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            sqlConn = dataSource.getConnection();
            ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureOfGroup());
            ps.setString(1, groupName);
            rs = ps.executeQuery();
            while (rs.next()) {
                Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
                mapFP.put(f.getUid(), f);
            }
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Returns Roles
            ps = sqlConn.prepareStatement(getQueryBuilder().getAllRoles());
            rs = ps.executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                // only feature in the group must be processed
                if (mapFP.containsKey(uid)) {
                    mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
                }
            }
            closeResultSet(rs);
            rs = null;
            closeStatement(ps);
            ps = null;

            // Read custom properties for each feature
            for (Feature f : mapFP.values()) {
                ps = sqlConn.prepareStatement(getQueryBuilder().getFeatureProperties());
                ps.setString(1, f.getUid());
                rs = ps.executeQuery();
                while (rs.next()) {
                    f.addProperty(JDBC_PROPERTY_MAPPER.map(rs));
                }
                closeResultSet(rs);
                rs = null;
                closeStatement(ps);
                ps = null;
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
    @Override
    public void addToGroup(String uid, String groupName) {
    	assertFeatureExist(uid);
        assertHasLength(groupName);
        update(getQueryBuilder().addFeatureToGroup(), groupName, uid);
    }

    /** {@inheritDoc} */
    @Override
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
    public void update(String query, Object... params) {
        Connection sqlConnection = null;
        PreparedStatement ps = null;
        try {
            sqlConnection = dataSource.getConnection();
            ps = buildStatement(sqlConnection, query, params);
            ps.executeUpdate();
            if (!sqlConnection.getAutoCommit()) {
		sqlConnection.commit();
	    }
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
