package org.ff4j.feature.repository;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import static org.ff4j.jdbc.JdbcUtils.buildStatement;
import static org.ff4j.jdbc.JdbcUtils.executeUpdate;
import static org.ff4j.jdbc.JdbcUtils.isTableExist;
import static org.ff4j.test.AssertUtils.assertHasLength;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import javax.sql.DataSource;

import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.jdbc.JdbcConstants.FeaturePermissionColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturePropertyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureToggleStrategyColumns;
import org.ff4j.jdbc.JdbcConstants.FeatureToggleStrategyPropertiesColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.jdbc.JdbcUtils;
import org.ff4j.jdbc.mapper.JdbcFeatureMapper;
import org.ff4j.jdbc.mapper.JdbcPropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.security.FF4jGrantees;
import org.ff4j.security.FF4jPermission;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link FeaturesRepository} to work with RDBMS through JDBC.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class FeaturesRepositoryJdbc extends FeaturesRepositorySupport {

	/** serialVersionUID. */
    private static final long serialVersionUID = 7144391802850457781L;

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

    /** Default Constructor. */
    public FeaturesRepositoryJdbc() {}

    /** {@inheritDoc} */
    public Stream<String> findAllIds() {
    	return null;
	}
    
    /**
     * Constructor from DataSource.
     *
     * @param jdbcDS
     *            native jdbc datasource
     */
    public FeaturesRepositoryJdbc(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        // Features
        if (!isTableExist(ds, qb.getTableNameFeatures())) {
            executeUpdate(ds, qb.sqlCreateTableFeatures());
        }
        // Permission
        if (!isTableExist(ds, qb.getTableNameFeaturePermission())) {
            executeUpdate(ds, qb.sqlCreateTableFeaturePermission());
        }
        // Properties
        if (!isTableExist(ds, qb.getTableNameFeatureProperties())) {
            executeUpdate(ds, qb.sqlCreateTableFeatureProperties());
        }
        // Toggle Strategy
        if (!isTableExist(ds, qb.getTableNameToggleStrategy())) {
            executeUpdate(ds, qb.sqlCreateTableToggleStrategy());
        }
        if (!isTableExist(ds, qb.getTableNameToggleStrategyProperties())) {
            executeUpdate(ds, qb.sqlCreateTableToggleStrategyProperties());
        }
        JdbcUtils.createSchemaSecurity(ds);
    }
    
    /** {@inheritDoc} */
    @Override
    public void toggleOn(String uid) {
        super.toggleOn(uid);
        // nothing to add, can be optimize with an update on 2 column.
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOnGroup(String groupName) {
        assertGroupExist(groupName);
        update(getQueryBuilder().sqlEditGroupStatus(), 1, groupName);
    }

    /** {@inheritDoc} */
    @Override
    public void toggleOffGroup(String groupName) {
        assertGroupExist(groupName);
        update(getQueryBuilder().sqlEditGroupStatus(), 0, groupName);
    }

    /** {@inheritDoc} */
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertHasLength(groupName);
        update(getQueryBuilder().sqlEditFeatureToGroup(), groupName, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        Feature f = read(uid);
        if (f.getGroup().isPresent() && groupName.equals(f.getGroup().get())) {
            update(getQueryBuilder().sqlEditFeatureToGroup(), "", uid);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public long count() {
        try (Connection sqlConn = getDataSource().getConnection()) {
            try (PreparedStatement ps = JdbcUtils.buildStatement(sqlConn, getQueryBuilder().sqlCountFeatures())) {
                try (ResultSet rs = ps.executeQuery()) {
                    //Query count always have return
                    rs.next();
                    return rs.getInt(1);
                }
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exists(String uid) {
        assertHasLength(uid);
    	try (Connection sqlConn = getDataSource().getConnection()) {
    	    try(PreparedStatement ps1 = JdbcUtils.buildStatement(sqlConn, getQueryBuilder().sqlExistFeature(), uid)) {
    	        try (ResultSet rs1 = ps1.executeQuery()) {
    	            // Query count always have return a value
                    rs1.next();
    	            return 1 == rs1.getInt(1);
    	        }
    	    }
    	} catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Optional < Feature > find(String uid) {
        assertHasLength(uid);
        Feature f = null;
        try (Connection sqlConn = getDataSource().getConnection()) {
            JdbcFeatureMapper  fmapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
            // CORE FEATURE
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlFindFeatureById())) {
                ps1.setString(1, uid);
                try (ResultSet rs1 = ps1.executeQuery()) {
                    if (!rs1.next()) {
                        return Optional.empty();
                    } else {
                        f = fmapper.mapFromRepository(rs1);
                    }
                }
            }
            // PROPERTIES
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectPropertiesForFeature())) {
                ps3.setString(1, uid);
                try (ResultSet rs3 = ps3.executeQuery()) {
                    while (rs3.next()) {
                        f.addProperty(pmapper.mapFeaturePropertyRepository(rs3));
                    }
                }
            } 
            
            // TOGGLE STRATEGY
            Map < String, Set < Property<?> > > toggleStratProperties = new HashMap<>();
            try(PreparedStatement psToggleStratParam = sqlConn.prepareStatement(getQueryBuilder().sqlSelectToggleStrategiesPropOfFeature())) {
                psToggleStratParam.setString(1, uid);
                try (ResultSet rsToggleStratParam = psToggleStratParam.executeQuery()) {
                    while (rsToggleStratParam.next()) {
                        String featureUid = rsToggleStratParam.getString(FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID.colname());
                        if (!toggleStratProperties.containsKey(featureUid)) {
                            toggleStratProperties.put(featureUid, new HashSet<>());
                        }
                        toggleStratProperties.get(featureUid).add(pmapper.mapFeaturePropertyRepository(rsToggleStratParam));
                    }   
                }
            }
            try(PreparedStatement psToggleStrategies = sqlConn.prepareStatement(getQueryBuilder().sqlSelectToggleStrategiesForFeature())) {
                psToggleStrategies.setString(1, uid);
                try (ResultSet rsToggleStrat = psToggleStrategies.executeQuery()) {
                    while (rsToggleStrat.next()) {
                        String toggleStrategyClassName = rsToggleStrat.getString(FeatureToggleStrategyColumns.TOGGLE_CLASS.colname());
                        f.addToggleStrategy(TogglePredicate.of(uid, toggleStrategyClassName, toggleStratProperties.get(uid)));
                    }
                }
            }
             
            // PERMISSION
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectPermissionsOfFeature())) {
                ps3.setString(1, uid);
                try (ResultSet rsPerm = ps3.executeQuery()) {
                    while (rsPerm.next()) {
                        FF4jGrantees grantees = new FF4jGrantees();
                        String permissionName = rsPerm.getString(FeaturePermissionColumns.PERMISSION.colname());
                        String userList       = rsPerm.getString(FeaturePermissionColumns.USERS.colname());
                        String roleList       = rsPerm.getString(FeaturePermissionColumns.ROLES.colname());
                        if (userList != null) {
                            grantees.getUsers().addAll(Arrays.asList(userList.split(",")));
                        }
                        if (roleList != null) {
                            grantees.getRoles().addAll(Arrays.asList(roleList.split(",")));
                        }
                        f.getAccessControlList().getPermissions().put(FF4jPermission.valueOf(permissionName), grantees);
                    }
                }
            }
            
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
        return Optional.ofNullable(f);
    }

    /** {@inheritDoc} */
    @Override
    public void deleteFeature(String uid) {
    	assertFeatureExist(uid);
        try (Connection sqlConn = getDataSource().getConnection()) {
            sqlConn.setAutoCommit(false);
            Feature fp = read(uid);
            // Properties
            try (PreparedStatement psProperties = sqlConn.prepareStatement(
                    getQueryBuilder().sqlDeleteAllPropertiesFromFeature())) {
                psProperties.setString(1, fp.getUid());
                psProperties.executeUpdate();
            }
            // Permissions
            try (PreparedStatement psPermissions = sqlConn.prepareStatement(
                    getQueryBuilder().sqlDeleteAllFeaturePermissionForFeature())) {
                psPermissions.setString(1, fp.getUid());
                psPermissions.executeUpdate();
            }
            // Toggle StrategiesParams
            try (PreparedStatement psToggleStrategyParams = sqlConn.prepareStatement(
                    getQueryBuilder().sqlDeleteAllToggleStrategyPropertiesForFeature())) {
                psToggleStrategyParams.setString(1, fp.getUid());
                psToggleStrategyParams.executeUpdate();
            }
            // Toggle Strategies
            try (PreparedStatement psToggleStrategy = sqlConn.prepareStatement(
                    getQueryBuilder().sqlDeleteAllToggleStrategieForFeature())) {
                psToggleStrategy.setString(1, fp.getUid());
                psToggleStrategy.executeUpdate();
            }
            // Core Feature
            try (PreparedStatement ps1 = sqlConn.prepareStatement(
                    getQueryBuilder().sqlDeleteFeature())) {
                ps1.setString(1, fp.getUid());
                ps1.executeUpdate();
            }
            // Commit
            sqlConn.commit();
            sqlConn.setAutoCommit(true);
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Stream < Feature > findAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        try (Connection sqlConn = getDataSource().getConnection()) {
        
            // CORE FEATURES
            JdbcFeatureMapper  fmapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlFindAllFeatures())) {
                try (ResultSet rs1 = ps1.executeQuery()) {
                    while (rs1.next()) {
                        Feature f = fmapper.mapFromRepository(rs1);
                        mapFP.put(f.getUid(), f);
                    }
                }
            }
            
            // FEATURE PROPERTIES
            JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllFeatureProperties())) {
                try (ResultSet rs3 = ps3.executeQuery()) {
                    while (rs3.next()) {
                        String featureId =  rs3.getString(FeaturePropertyColumns.FEATURE.colname());
                        mapFP.get(featureId).addProperty(pmapper.mapFeaturePropertyRepository(rs3));
                    }   
                }
            }
            
            // FEATURE TOGGLE STRATEGIES
            Map < String, Map < String, Set < Property<?> > > > toggleStratProperties = new HashMap<>();
            try(PreparedStatement psToggleStratParam = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllToggleStrategiesProperties())) {
                try (ResultSet rsToggleStratParam = psToggleStratParam.executeQuery()) {
                    while (rsToggleStratParam.next()) {
                        String featureUid      = rsToggleStratParam.getString(
                                FeatureToggleStrategyPropertiesColumns.STRAT_FEAT_UID.colname());
                        String toggleClassName = rsToggleStratParam.getString(
                                FeatureToggleStrategyPropertiesColumns.STRAT_CLASS.colname());
                        if (!toggleStratProperties.containsKey(featureUid)) {
                            toggleStratProperties.put(featureUid, new HashMap<>());
                        }
                        if (!toggleStratProperties.get(featureUid).containsKey(toggleClassName)) {
                            toggleStratProperties.get(featureUid).put(toggleClassName, new HashSet<>());
                        }
                        toggleStratProperties.get(featureUid).get(toggleClassName).add(pmapper.mapFeaturePropertyRepository(rsToggleStratParam));
                    }   
                }
            }
            try(PreparedStatement psToggleStrat = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllToggleStrategies())) {
                try (ResultSet rsToggleStrat = psToggleStrat.executeQuery()) {
                    while (rsToggleStrat.next()) {
                        String featureUid              = rsToggleStrat.getString(FeatureToggleStrategyColumns.FEATURE_UID.colname());
                        String toggleStrategyClassName = rsToggleStrat.getString(FeatureToggleStrategyColumns.TOGGLE_CLASS.colname());
                        mapFP.get(featureUid).addToggleStrategy(
                                TogglePredicate.of(featureUid, toggleStrategyClassName, 
                                        toggleStratProperties.get(featureUid).get(toggleStrategyClassName)));
                    }
                }
            }
            
            // FEATURE PERMISSIONS
            try(PreparedStatement psPerm = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllFeaturePermissions())) {
                try (ResultSet rsPerm = psPerm.executeQuery()) {
                    while (rsPerm.next()) {
                        FF4jGrantees grantees = new FF4jGrantees();
                        String relatedFeature = rsPerm.getString(FeaturePermissionColumns.FEAT_UID.colname());
                        String permissionName = rsPerm.getString(FeaturePermissionColumns.PERMISSION.colname());
                        String userList       = rsPerm.getString(FeaturePermissionColumns.USERS.colname());
                        String roleList       = rsPerm.getString(FeaturePermissionColumns.ROLES.colname());
                        if (userList != null) {
                            grantees.getUsers().addAll(Arrays.asList(userList.split(",")));
                        }
                        if (roleList != null) {
                            grantees.getRoles().addAll(Arrays.asList(roleList.split(",")));
                        }
                        mapFP.get(relatedFeature)
                             .getAccessControlList()
                             .getPermissions()
                             .put(FF4jPermission.valueOf(permissionName), grantees);
                    }
                }
            }
            
            return mapFP.values().stream();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        try (Connection sqlConn = getDataSource().getConnection()) {
            sqlConn.setAutoCommit(false);
            // Toggle Strategy properties
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllToggleStrategyProperties())) {
                ps1.executeUpdate();
            }
            // Toggle Strategy 
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllToggleStrategies())) {
                ps1.executeUpdate();
            }
            // Properties
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllCustomProperties())) {
                ps1.executeUpdate();
            }
            // Permissions
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllFeaturePermission())) {
                ps1.executeUpdate();
            }
            // Features
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllFeatures())) {
                ps3.executeUpdate();
            }
            // Commit
            sqlConn.commit();
            sqlConn.setAutoCommit(true);
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        assertHasLength(groupName);
        try (Connection sqlConn = dataSource.getConnection()) {
            try(PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().sqlExistGroup())) {
                ps.setString(1, groupName);
                try(ResultSet rs = ps.executeQuery()) {
                    rs.next();
                    return rs.getInt(1) > 0;
                }
            }   
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Stream <Feature> readGroup(String groupName) {
    	assertGroupExist(groupName);
    	List<Feature> listOfFeatures = new ArrayList<>();
    	try (Connection sqlConn = dataSource.getConnection()) {
    	    try(PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().sqlSelectFeaturesOfGroup())) {
                ps.setString(1, groupName);
                try(ResultSet rs = ps.executeQuery()) {
                    while (rs.next()) {
                        listOfFeatures.add(read(rs.getString(FeaturesColumns.UID.colname())));
                    }
                }
            } 
            return listOfFeatures.stream();
    	} catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void saveFeature(Feature feature) {
        assertFeatureNotNull(feature);
        assertHasLength(feature.getUid());
        if (exists(feature.getUid())) {
            deleteFeature(feature.getUid());
        }
        try (Connection sqlConn = dataSource.getConnection()) {
            sqlConn.setAutoCommit(false);
            JdbcFeatureMapper mapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            // Create FeatureCore
            try (PreparedStatement ps1 = mapper.mapToRepository(feature)) {
                ps1.executeUpdate();
            }
            // Create FeatureProperties
            if (!feature.getProperties().isEmpty()) {
                for(Property<?> property : feature.getProperties().values()) {
                    try(PreparedStatement ps = mapper.insertFeaturePropertyStatement(property, feature.getUid())) {
                        ps.executeUpdate();
                    }
                }
            }
            // Create FeaturePermission
            if (!feature.getAccessControlList().getPermissions().isEmpty()) {
                for(Map.Entry<FF4jPermission,FF4jGrantees> entry : feature.getAccessControlList().getPermissions().entrySet()) {
                    try(PreparedStatement ps = mapper.insertFeaturePermissionStatement(
                            feature.getUid(), entry.getKey(), entry.getValue())) {
                        ps.executeUpdate();
                    }
                }
            }
            // Create ToggleStrategy
            if (!feature.getToggleStrategies().isEmpty()) {
                for(TogglePredicate tp : feature.getToggleStrategies()) {
                    try(PreparedStatement ps = mapper.insertToggleStrategyStatement(feature.getUid(), tp)) {
                        ps.executeUpdate();
                    }
                    // Create ToggleStrategyProperties
                    for(Property<?> pf : tp.getPropertiesAsMap().values()) {
                        try(PreparedStatement ps = mapper.insertToggleStrategyPropertyStatement(feature.getUid(), tp, pf)) {
                            ps.executeUpdate();
                        }
                    }
                }
            }
            // Commit
            sqlConn.commit();
            sqlConn.setAutoCommit(true);
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Stream<String> listGroupNames() {
        Set<String> setOFGroup = new HashSet<String>();
        try (Connection sqlConn = getDataSource().getConnection()) {
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllGroups())) {
                try (ResultSet rs1 = ps1.executeQuery()) {
                    while (rs1.next()) {
                        String groupName = rs1.getString(FeaturesColumns.GROUPNAME.colname());
                        if (Util.hasLength(groupName)) {
                            setOFGroup.add(groupName);
                        }  
                    }
                }
            }
            return setOFGroup.stream();
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot list groups, error related to database", sqlEX);
        }
    }

    
    /**
     * Utility method to perform UPDATE and DELETE operations.
     *
     * @param query
     *            target query
     * @param params
     *            sql query params
     */
    private void update(String query, Object... params) {
        try (Connection sqlConn = dataSource.getConnection()) {
            try (PreparedStatement ps = buildStatement(sqlConn, query, params)) {
                ps.executeUpdate();
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
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
