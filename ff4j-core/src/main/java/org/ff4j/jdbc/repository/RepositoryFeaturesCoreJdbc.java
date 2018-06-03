package org.ff4j.jdbc.repository;

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
import static org.ff4j.jdbc.JdbcUtils.closeConnection;
import static org.ff4j.jdbc.JdbcUtils.executeUpdate;
import static org.ff4j.jdbc.JdbcUtils.isTableExist;
import static org.ff4j.test.AssertUtils.assertHasLength;
import static org.ff4j.test.AssertUtils.assertNotNull;

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.util.Collection;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Optional;
import java.util.Set;
import java.util.stream.Stream;

import javax.sql.DataSource;

import org.ff4j.feature.Feature;
import org.ff4j.feature.AbstractRepositoryFeatures;
import org.ff4j.feature.RepositoryFeatures;
import org.ff4j.feature.ToggleStrategy;
import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.jdbc.JdbcConstants.FeaturePropertyColumns;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.jdbc.JdbcUtils;
import org.ff4j.jdbc.mapper.JdbcFeatureMapper;
import org.ff4j.jdbc.mapper.JdbcFeatureToggleStrategyMapper;
import org.ff4j.jdbc.mapper.JdbcPropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.Util;

/**
 * Implementation of {@link RepositoryFeatures} to work with RDBMS through JDBC.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class RepositoryFeaturesCoreJdbc extends AbstractRepositoryFeatures {

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
    public RepositoryFeaturesCoreJdbc() {}

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
    public RepositoryFeaturesCoreJdbc(DataSource jdbcDS) {
        this.dataSource = jdbcDS;
    }

    /**
     * Constructor from DataSource.
     *
     * @param jdbcDS
     *            native jdbc datasource
     */
    public RepositoryFeaturesCoreJdbc(DataSource jdbcDS, String xmlConfFile) {
        this(jdbcDS);
        importFeaturesFromXmlFile(xmlConfFile);
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        DataSource       ds = getDataSource();
        JdbcQueryBuilder qb = getQueryBuilder();
        // Core Features
        if (!isTableExist(ds, qb.getTableNameFeatures())) {
            executeUpdate(ds, qb.sqlCreateTableFeatures());
        }
        // Strategies from features
        if (!isTableExist(ds, qb.getTableNameFeatureStrategy())) {
            executeUpdate(ds, qb.sqlCreateTableToggleStrategy());
        }
        // Properties from features
        if (!isTableExist(ds, qb.getTableNameFeatureProperties())) {
            executeUpdate(ds, qb.sqlCreateTableFeatureProperties());
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
    @Override
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
        Feature feat = read(uid);
        if (feat.getGroup().isPresent() && !feat.getGroup().get().equals(groupName)) {
            throw new IllegalArgumentException("'" + uid + "' is not in group '" + groupName + "'");
        }
        update(getQueryBuilder().sqlEditFeatureToGroup(), "", uid);
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
    public Optional < Feature > findById(String uid) {
        assertHasLength(uid);
        // Closeable sql connection
        Feature f = null;
        try (Connection sqlConn = getDataSource().getConnection()) {
            JdbcFeatureMapper  fmapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
            // Get core feature
            try(PreparedStatement ps1 = sqlConn.prepareStatement(
                    getQueryBuilder().sqlFindFeatureById())) {
                ps1.setString(1, uid);
                try (ResultSet rs1 = ps1.executeQuery()) {
                    if (!rs1.next()) {
                        return Optional.empty();
                    } else {
                        f = fmapper.mapFromRepository(rs1);
                    }
                }
            }
            
            
            // Get strategies related to features
            try(PreparedStatement ps2 = sqlConn.prepareStatement(getQueryBuilder().sqlStrategyOfFeature())) {
                
            }
            
            /* Get AccessControlList related to features
            try(PreparedStatement ps2 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectFeatureAccessControlList())) {
                ps2.setString(1, uid);
                try (ResultSet rs2 = ps2.executeQuery()) {
                    while (rs2.next()) {
                        f.addPermissions(rs2.getString(RolesColumns.ROLE.colname()));
                    }   
                }
            }*/
            
            // Get Custom properties related to features
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectCustomPropertiesOfFeature())) {
                ps3.setString(1, uid);
                try (ResultSet rs3 = ps3.executeQuery()) {
                    while (rs3.next()) {
                        f.addCustomProperty(pmapper.mapFromRepository(rs3));
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
    public void delete(String uid) {
    	assertFeatureExist(uid);
        try (Connection sqlConn = getDataSource().getConnection()) {
            sqlConn.setAutoCommit(false);
            Feature fp = read(uid);
            if (fp.getCustomProperties().isPresent()) {
                try (PreparedStatement ps1 = 
                        sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllCustomPropertiesOfFeature())) {
                    ps1.setString(1, fp.getUid());
                    ps1.executeUpdate();
                }
            }
            /* Delete Roles
            if (fp.getPermissions().isPresent()) {
                try (PreparedStatement ps1 = 
                        sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllRolesOfFeature())) {
                    ps1.setString(1, fp.getUid());
                    ps1.executeUpdate();
                }
            }*/
            // Delete Feature
            try (PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteFeature())) {
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
        // Closeable sql connection
        try (Connection sqlConn = getDataSource().getConnection()) {
            // Get core feature
            JdbcFeatureMapper  fmapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlFindAllFeatures())) {
                try (ResultSet rs1 = ps1.executeQuery()) {
                    while (rs1.next()) {
                        Feature f = fmapper.mapFromRepository(rs1);
                        mapFP.put(f.getUid(), f);
                    }
                }
            }
            /* Get Roles related to features
            try(PreparedStatement ps2 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllRoles())) {
                try (ResultSet rs2 = ps2.executeQuery()) {
                    while (rs2.next()) {
                        mapFP.get(rs2.getString(RolesColumns.FEATURE_UID.colname()))
                            .addPermission(rs2.getString(RolesColumns.ROLE.colname()));
                    }   
                }
            }*/
            // Get Custom properties related to features
            JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllCustomProperties())) {
                try (ResultSet rs3 = ps3.executeQuery()) {
                    while (rs3.next()) {
                        mapFP.get(rs3.getString(FeaturePropertyColumns.UID.colname()))
                             .addCustomProperty(pmapper.mapFromRepository(rs3));
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
    public Stream<String> listAllGroupNames() {
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

    /** {@inheritDoc} */
    @Override
    public void deleteAll() {
        try (Connection sqlConn = getDataSource().getConnection()) {
            try(PreparedStatement ps1 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllCustomProperties())) {
                ps1.executeUpdate();
            }
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlDeleteAllFeatures())) {
                ps3.executeUpdate();
            }
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_CHECK_FEATURE_EXISTENCE_ERROR_RELATED_TO_DATABASE, sqlEX);
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
        assertHasLength(uid);
        if (props == null) return;
        try (Connection sqlConn = getDataSource().getConnection()) {
            // Begin TX
            sqlConn.setAutoCommit(false);
            // Queries
            for (Property<?> pp : props) {
                JdbcPropertyMapper mapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
                try(PreparedStatement ps = mapper.customPropertytoStore(pp, uid)) {
                    ps.executeUpdate();
                }
            }
            // End TX
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
    	LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
    	try (Connection sqlConn = dataSource.getConnection()) {
            // Feature Core
    	    JdbcFeatureMapper mapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
    	    try(PreparedStatement ps = sqlConn.prepareStatement(getQueryBuilder().sqlSelectFeaturesOfGroup())) {
                ps.setString(1, groupName);
                try(ResultSet rs = ps.executeQuery()) {
                    while (rs.next()) {
                        Feature f = mapper.mapFromRepository(rs);
                        mapFP.put(f.getUid(), f);
                    }
                }
            }
    	    /* Get Roles related to features
            try(PreparedStatement ps2 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllRoles())) {
                try (ResultSet rs2 = ps2.executeQuery()) {
                    while (rs2.next()) {
                        String featureId = rs2.getString(RolesColumns.FEATURE_UID.colname());
                        if (mapFP.containsKey(featureId)) {
                            mapFP.get(featureId).addPermission(rs2.getString(RolesColumns.ROLE.colname()));
                        }
                    }   
                }
            }*/
            // Get Custom properties related to features
            JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
            try(PreparedStatement ps3 = sqlConn.prepareStatement(getQueryBuilder().sqlSelectAllCustomProperties())) {
                try (ResultSet rs3 = ps3.executeQuery()) {
                    while (rs3.next()) {
                        String featureId = rs3.getString(FeaturePropertyColumns.UID.colname());
                        if (mapFP.containsKey(featureId)) {
                            mapFP.get(featureId).addCustomProperty(pmapper.mapFromRepository(rs3));
                        }
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
    public void update(Feature entity) {
        entity.setLastModified(LocalDateTime.now());
        entity.setCreationDate(entity.getCreationDate().orElse(entity.getLastModifiedDate().get()));
        delete(entity);
        assertFeatureNotExist(entity.getUid());
        create(entity);
    }
    
    /** {@inheritDoc} */
    @Override
    protected void createFeature(Feature feature) {
        assertFeatureNotNull(feature);
        assertFeatureNotExist(feature.getUid());
        Connection sqlConn = null;
        try {
            // Create Connection
            sqlConn = getDataSource().getConnection();
            sqlConn.setAutoCommit(false);
            
            // Create Core Feature
            JdbcFeatureMapper mapper = new JdbcFeatureMapper(sqlConn, getQueryBuilder());
            try (PreparedStatement ps1 = mapper.mapToRepository(feature)) {
                ps1.executeUpdate();
            }
            
            // Create Toggle Strategies
            if (null != feature.getToggleStrategies()) {
                JdbcFeatureToggleStrategyMapper tmapper = new JdbcFeatureToggleStrategyMapper(sqlConn, getQueryBuilder(), feature.getUid());
                for(ToggleStrategy ts : feature.getToggleStrategies()) {
                    try(PreparedStatement ps = tmapper.mapToRepository(ts)) {
                        ps.executeUpdate();
                    }
                }
            }
            
            // Create Custom Properties
            if (feature.getCustomProperties().isPresent()) {
                JdbcPropertyMapper pmapper = new JdbcPropertyMapper(sqlConn, getQueryBuilder());
                for(Property<?> property : feature.getCustomProperties().get().values()) {
                    try(PreparedStatement ps = pmapper.customPropertytoStore(property, feature.getUid())) {
                        ps.executeUpdate();
                    }
                }
            }
           
            // Create Permissions
            if (feature.getAccessControlList() != null && !feature.getAccessControlList().isEmpty()) {
                
            }            
            
            // Commit
            sqlConn.commit();
            sqlConn.setAutoCommit(true);
            
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException(CANNOT_UPDATE_FEATURES_DATABASE_SQL_ERROR, sqlEX);
        } finally {
            closeConnection(sqlConn);
        }
    }

    /** {@inheritDoc} */
    @Override
    protected void updateFeature(Feature feature) {
        assertNotNull(feature);
        assertHasLength(feature.getUid());
        assertItemExist(feature.getUid());
    }

    /** {@inheritDoc} */
    @Override
    protected void deleteFeature(String uid) {
    }

    /** {@inheritDoc} */
    @Override
    protected void deleteAllFeatures() {
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
