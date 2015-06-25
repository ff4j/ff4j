package org.ff4j.store;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

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
import org.ff4j.property.AbstractProperty;
import org.ff4j.property.store.JdbcPropertyMapper;
import org.ff4j.utils.ParameterUtils;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureStore extends AbstractFeatureStore implements  JdbcStoreConstants {

    /** Access to storage. */
    private DataSource dataSource;
    
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
    public void enable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        update(SQL_ENABLE, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        update(SQL_DISABLE, uid);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = buildStatement(SQL_EXIST, uid);
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
    @SuppressWarnings("resource")
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            ps = buildStatement(SQLQUERY_GET_FEATURE_BY_ID, uid);
            rs = ps.executeQuery();
            Feature f = null;
            if (rs.next()) {
                f = JDBC_FEATURE_MAPPER.mapFeature(rs);
            } else {
                throw new FeatureNotFoundException(uid);
            }

            // 2nd request
            ps = ps.getConnection().prepareStatement(SQL_GET_ROLES);
            ps.setString(1, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
                f.getPermissions().add(rs.getString("ROLE_NAME"));
            }
            
            // 3d request to get custom properties by uid
            ps = buildStatement(SQL_GET_CUSTOMPROPERTIES_BYFEATUREID, uid);
            rs = ps.executeQuery();
            while (rs.next()) {
                AbstractProperty<?> ap = JDBC_PROPERTY_MAPPER.map(rs);
                f.getCustomProperties().put(ap.getName(), ap);
            }
            return f;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }  
    

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {

            // Create connection
            sqlConn = getDataSource().getConnection();
            sqlConn.setAutoCommit(false);

            // Create feature
            ps = sqlConn.prepareStatement(SQL_CREATE);
            int idx = 1;
            ps.setString(idx++, fp.getUid());
            ps.setInt(idx++, fp.isEnable() ? 1 : 0);
            ps.setString(idx++, fp.getDescription());

            String strategyColumn = null;
            String expressionColumn = null;
            if (fp.getFlippingStrategy() != null) {
                strategyColumn = fp.getFlippingStrategy().getClass().getCanonicalName();
                expressionColumn = ParameterUtils.fromMap(fp.getFlippingStrategy().getInitParams());
            }
            ps.setString(idx++, strategyColumn);
            ps.setString(idx++, expressionColumn);
            ps.setString(idx++, fp.getGroup());
            ps.executeUpdate();

            // Create roles
            if (fp.getPermissions() != null) {
                for (String role : fp.getPermissions()) {
                    ps = sqlConn.prepareStatement(SQL_ADD_ROLE);
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
                    ps.executeUpdate();
                }
            }
            
            // Create customproperties
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                for (AbstractProperty<?> pp : fp.getCustomProperties().values()) {
                    ps = sqlConn.prepareStatement(SQL_CREATE_CUSTOMPROPERTY);
                    ps.setString(1, pp.getName());
                    ps.setString(2, pp.getType());
                    ps.setString(3, pp.asString());
                    if (pp.getFixedValues() != null && pp.getFixedValues().size() > 0) {
                        String fixedValues = pp.getFixedValues().toString();
                        ps.setString(4, fixedValues.substring(1, fixedValues.length() - 1));
                    } else {
                        ps.setString(4, null);
                    }
                    
                    ps.setString(5, fp.getUid());
                    ps.executeUpdate();
                }
            }

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @SuppressWarnings("resource")
    @Override
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier (param#0) cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {
            Feature fp = read(uid);

            // Create connection
            sqlConn = getDataSource().getConnection();
            sqlConn.setAutoCommit(false);
            
            // Delete Properties
            if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
                for (String property : fp.getCustomProperties().keySet()) {
                    ps = sqlConn.prepareStatement(SQL_DELETE_CUSTOMPROPERTY);
                    ps.setString(1, property);
                    ps.setString(2, fp.getUid());
                    ps.executeUpdate();
                }
            }

            // Delete Roles
            if (fp.getPermissions() != null) {
                for (String role : fp.getPermissions()) {
                    ps = sqlConn.prepareStatement(SQL_DELETE_ROLE);
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
                    ps.executeUpdate();
                }
            }

            // Delete Feature
            ps = sqlConn.prepareStatement(SQL_DELETE);
            ps.setString(1, fp.getUid());
            ps.executeUpdate();

            // Commit
            sqlConn.commit();

        } catch (SQLException sqlEX) {
            rollback(sqlConn);
            throw new FeatureAccessException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
        }
    }

    /**
     * Utility method to perform rollback in correct way.
     * 
     * @param sqlConn
     *            current sql connection
     */
    private void rollback(Connection sqlConn) {
        try {
            if (!sqlConn.isClosed()) {
                sqlConn.rollback();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("Cannot rollback database, SQL ERROR", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        update(SQL_ADD_ROLE, uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        update(SQL_DELETE_ROLE, uid, roleName);
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

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            ps = buildStatement(SQLQUERY_ALLFEATURES);
            rs = ps.executeQuery();
            while (rs.next()) {
                Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(SQL_GET_ALLROLES).executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
            }
            return mapFP;

        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> setOFGroup = new HashSet<String>();
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            ps = buildStatement(SQLQUERY_ALLGROUPS);
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
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        Feature fpExist = read(fp.getUid());
        String enable = "0";
        if (fp.isEnable()) {
            enable = "1";
        }
        String fStrategy = null;
        String fExpression = null;
        if (fp.getFlippingStrategy() != null) {
            fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
            fExpression = ParameterUtils.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        update(SQL_UPDATE, enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(), fp.getUid());

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
            PreparedStatement ps = null;
            ResultSet rs = null;
            try {
                ps = buildStatement(SQL_DELETE_CUSTOMPROPERTIES, fpExist.getUid());
                ps.executeUpdate();
            } catch (SQLException sqlEX) {
                throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
            } finally {
                closeResultSet(rs);
                closeStatement(ps);
            }
        }
        
        // CREATE PROPERTIES
        if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
            createCustomProperties(fp.getUid(), fp.getCustomProperties().values());
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
    private void createCustomProperties(String uid, Collection <AbstractProperty<?> > props) {
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            for (AbstractProperty<?> pp : props) {
                ps = buildStatement(SQL_CREATE_CUSTOMPROPERTY);
                ps.setString(1, pp.getName());
                ps.setString(2, pp.getType());
                ps.setString(3, pp.asString());
                if (pp.getFixedValues() != null && pp.getFixedValues().size() > 0) {
                    String fixedValues = pp.getFixedValues().toString();
                    ps.setString(4, fixedValues.substring(1, fixedValues.length() - 1));
                } else {
                    ps.setString(4, null);
                }
                ps.setString(5, uid);
                ps.executeUpdate();
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = buildStatement(SQL_EXIST_GROUP, groupName);
            rs = ps.executeQuery();
            if (rs.next()) {
                return (rs.getInt(1) > 0);
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
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        update(SQL_ENABLE_GROUP, groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        update(SQL_DISABLE_GROUP, groupName);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            ps = buildStatement(SQLQUERY_GET_FEATURE_GROUP, groupName);
            rs = ps.executeQuery();
            while (rs.next()) {
                Feature f = JDBC_FEATURE_MAPPER.mapFeature(rs);
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(SQL_GET_ALLROLES).executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                // only feature in the group must be processed
                if (mapFP.containsKey(uid)) {
                    mapFP.get(uid).getPermissions().add(rs.getString(COL_ROLE_ROLENAME));
                }
            }
            return mapFP;

        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        update(SQL_ADD_TO_GROUP, groupName, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        Feature feat = read(uid);
        if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
            throw new IllegalArgumentException("'" + uid + "' is not in group '" + groupName + "'");
        }
        update(SQL_ADD_TO_GROUP, "", uid);
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
        sb.append("\"datasource\":\"" + this.dataSource.getClass() + "\"");
        sb.append(",\"cached\":" + this.isCached());
        if (this.isCached()) {
            sb.append(",\"cacheProvider\":\"" + this.getCacheProvider() + "\"");
            sb.append(",\"cacheStore\":\"" + this.getCachedTargetStore() + "\"");
        }
        Set<String> myFeatures = readAll().keySet();
        sb.append(",\"numberOfFeatures\":" + myFeatures.size());
        sb.append(",\"features\":[");
        boolean first = true;
        for (String myFeature : myFeatures) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"" + myFeature + "\"");
        }
        Set<String> myGroups = readAllGroups();
        sb.append("],\"numberOfGroups\":" + myGroups.size());
        sb.append(",\"groups\":[");
        first = true;
        for (String myGroup : myGroups) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"" + myGroup + "\"");
        }
        sb.append("]");
        sb.append("}");
        return sb.toString();
    }

    // -------- Overrided in cache proxy --------------

    /** {@inheritDoc} */
    @Override
    public boolean isCached() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProvider() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getCachedTargetStore() {
        return null;
    }

    /**
     * Utility method to perform UPDATE and DELETE operations.
     * 
     * @param query
     *            target query
     * @param params
     *            sql query params
     */
    private void update(String query, String... params) {
        PreparedStatement ps = null;
        try {
            ps = buildStatement(query, params);
            ps.executeUpdate();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            closeStatement(ps);
        }
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
            if (ps != null) {
                if (ps.getConnection() != null) {
                    ps.getConnection().close();
                }
                ps.close();
            }
        } catch (SQLException e) {
            throw new FeatureAccessException("An error occur when closing statement", e);
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

}
