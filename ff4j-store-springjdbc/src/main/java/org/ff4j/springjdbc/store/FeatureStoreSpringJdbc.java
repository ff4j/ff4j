package org.ff4j.springjdbc.store;

/*
 * #%L ff4j-store-jdbc %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not
 * use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.springjdbc.store.rowmapper.CustomPropertyRowMapper;
import org.ff4j.springjdbc.store.rowmapper.FeatureRowMapper;
import org.ff4j.springjdbc.store.rowmapper.RoleRowMapper;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.JdbcUtils;
import org.ff4j.utils.MappingUtil;
import org.ff4j.utils.Util;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.dao.EmptyResultDataAccessException;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.SingleColumnRowMapper;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Repository
public class FeatureStoreSpringJdbc extends AbstractFeatureStore {

    /** Row Mapper for FlipPoint. */
    private static final FeatureRowMapper FMAPPER = new FeatureRowMapper();

    /** Mapper for custom properties. */
    private static final CustomPropertyRowMapper PMAPPER = new CustomPropertyRowMapper();
    
    /** Error message. */
    public static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";
    
    /** Error message. */
    public static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;
    
    /** Query builder. */
    private JdbcQueryBuilder queryBuilder;
    
    /**
     * Default constructor.
     */
    public FeatureStoreSpringJdbc() {
    }
    
    /**
     * Default constructor.
     */
    public FeatureStoreSpringJdbc(DataSource ds) {
        this.dataSource = ds;
    }

    /** {@inheritDoc} */
    public void enable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(getQueryBuilder().enableFeature(), uid);
    }

    /** {@inheritDoc} */
    public void disable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(getQueryBuilder().disableFeature(), uid);
    }

    /** {@inheritDoc} */
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return 1 == getJdbcTemplate().queryForObject(getQueryBuilder().existFeature(), Integer.class, uid);
    }

    /** {@inheritDoc} */
    public Feature read(String uid) {
        Util.assertHasLength(uid);
        try {
            Feature feature = getJdbcTemplate().queryForObject(
            		getQueryBuilder().getFeature(), FMAPPER, uid);
            readProperties(feature);
            readPermissions(feature);
            return feature;
        } catch(EmptyResultDataAccessException ex) { 
            throw new FeatureNotFoundException(uid, ex);
        }
    }
    
    /**
     * Query children properties.
     *
     * @param fp
     */
    private void readProperties(Feature fp) {
        List<Property<?>> listOfProps = getJdbcTemplate().query(
        		getQueryBuilder().getFeatureProperties(), PMAPPER, fp.getUid());
        for (Property<?> ap : listOfProps) {
            fp.getCustomProperties().put(ap.getName(), ap);
        }
    }
    
    /**
     * Query children roles.
     *
     * @param fp
     */
    private void readPermissions(Feature fp) {
        fp.getPermissions().addAll(
                getJdbcTemplate().query(getQueryBuilder().getRoles(), 
                        new SingleColumnRowMapper<String>(), fp.getUid()));
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void create(Feature fp) {
        Util.assertNotNull(fp);
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        createCoreFeature(fp);
        createPermissions(fp);
        createProperties(fp);
    }
    
    private void createCoreFeature(Feature fp) {
        // Transaction wraps the method, could pipe several sql queries
        String strategyColumn   = null;
        String expressionColumn = null;
        if (fp.getFlippingStrategy() != null) {
            strategyColumn = fp.getFlippingStrategy().getClass().getName();
            expressionColumn = MappingUtil.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        getJdbcTemplate().update(getQueryBuilder().createFeature(), 
                fp.getUid(), fp.isEnable() ? 1 : 0, 
                fp.getDescription(), strategyColumn,
                expressionColumn, fp.getGroup());
    }
    
    /**
     * Remove all existing permissions and create new.
     *
     * @param fp
     */
    private void createPermissions(Feature fp) {
        if (fp.getPermissions() != null) {
            getJdbcTemplate().update(getQueryBuilder().deleteRoles(), fp.getUid());
            for (String role : fp.getPermissions()) {
                getJdbcTemplate().update(getQueryBuilder().addRoleToFeature(), fp.getUid(), role);
            }
        }
    }
    
    /**
     * Remove all existing permissions and create new.
     *
     * @param fp
     */
    private void createProperties(Feature fp) {
        if (fp.getCustomProperties() != null) {
            getJdbcTemplate().update(getQueryBuilder().deleteAllFeatureCustomProperties(), fp.getUid());
            for (String propertyName : fp.getCustomProperties().keySet()) {
                Property<?> ap = fp.getCustomProperties().get(propertyName);
                String fixedValues = null;
                if (ap.getFixedValues() != null && ap.getFixedValues().size() > 0) {
                    fixedValues = ap.getFixedValues().toString();
                    fixedValues = fixedValues.substring(1, fixedValues.length() - 1);
                }
                getJdbcTemplate().update(getQueryBuilder().createFeatureProperty(), 
                        ap.getName(), ap.getType(), ap.asString(), 
                        ap.getDescription(), fixedValues, fp.getUid());
            }
        } 
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void delete(String uid) {
        if (!exist(uid)) throw new FeatureNotFoundException(uid);
        deletePermissions(uid);
        deleteProperties(uid);
        deleteCoreFeature(uid);
    }
    
    /**
     * Delete permissions related to a feature.
     *
     * @param featureId
     *      current feature uid
     */
    private void deletePermissions(String featureId) {
        getJdbcTemplate().update(getQueryBuilder().deleteRoles(), featureId);
    }
    
    /**
     * Delete properties related to a feature.
     *
     * @param featureId
     *      current feature uid
     */
    private void deleteProperties(String featureId) {
        getJdbcTemplate().update(getQueryBuilder().deleteAllFeatureCustomProperties(), featureId);
    }
    
    /**
     * Delete core feature.
     *
     * @param featureId
     *      current feature uid
     */
    private void deleteCoreFeature(String featureId) {
        getJdbcTemplate().update(getQueryBuilder().deleteFeature(), featureId);
    }

    /** {@inheritDoc} */
    @Transactional
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(getQueryBuilder().addRoleToFeature(), uid, roleName);
    }

    /** {@inheritDoc} */
    @Transactional
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(getQueryBuilder().deleteFeatureRole(), uid, roleName);
    }

    /** {@inheritDoc} */
    @Transactional
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        getJdbcTemplate().update(getQueryBuilder().enableGroup(), groupName);
    }

    /** {@inheritDoc} */
    @Transactional
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        getJdbcTemplate().update(getQueryBuilder().disableGroup(), groupName);
    }

    /** {@inheritDoc} */
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        int count = getJdbcTemplate().queryForObject(
        		getQueryBuilder().existGroup(), Integer.class, groupName);
        return count > 0;
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        List<Feature> lFp = getJdbcTemplate().query(getQueryBuilder().getFeatureOfGroup(), FMAPPER, groupName);
        for (Feature flipPoint : lFp) {
            mapFP.put(flipPoint.getUid(), flipPoint);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Transactional
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(getQueryBuilder().addFeatureToGroup(), groupName, uid);
    }

    /** {@inheritDoc} */
    @Transactional
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        getJdbcTemplate().update(getQueryBuilder().addFeatureToGroup(), "", uid);
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        List<Feature> lFp = getJdbcTemplate().query(getQueryBuilder().getAllFeatures(), FMAPPER);
        for (Feature flipPoint : lFp) {
            mapFP.put(flipPoint.getUid(), flipPoint);
        }
        // Populating Roles
        RoleRowMapper rrm = new RoleRowMapper();
        getJdbcTemplate().query(getQueryBuilder().getAllRoles(), rrm);
        Map<String, Set<String>> roles = rrm.getRoles();
        for (Map.Entry<String,Set<String>> featId : roles.entrySet()) {
            if (mapFP.containsKey(featId.getKey())) {
                mapFP.get(featId.getKey()).getPermissions().addAll(featId.getValue());
            }
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    public Set<String> readAllGroups() {
        Set<String> setOfGroup = new HashSet<String>();
        setOfGroup.addAll(getJdbcTemplate().query(
        		getQueryBuilder().getAllGroups(), new SingleColumnRowMapper<String>()));
        setOfGroup.remove(null);
        setOfGroup.remove("");
        return setOfGroup;
    }

    /** {@inheritDoc} */
    @Transactional
    public void update(Feature newFeature) {
        Util.assertNotNull(newFeature);
        delete(newFeature.getUid());
        create(newFeature);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void createSchema() {
        JdbcQueryBuilder qb = getQueryBuilder();
        if (!JdbcUtils.isTableExist(dataSource, qb.getTableNameFeatures())) {
            getJdbcTemplate().update(qb.sqlCreateTableFeatures());
        }
        if (!JdbcUtils.isTableExist(dataSource, qb.getTableNameCustomProperties())) {
            getJdbcTemplate().update(qb.sqlCreateTableCustomProperties());
        }
        if (!JdbcUtils.isTableExist(dataSource, qb.getTableNameRoles())) {
            getJdbcTemplate().update(qb.sqlCreateTableRoles());
        }
    }
    
    /** {@inheritDoc} */
    @Transactional
    public void clear() {
        getJdbcTemplate().update(getQueryBuilder().deleteAllRoles());
        getJdbcTemplate().update(getQueryBuilder().deleteAllCustomProperties());
        getJdbcTemplate().update(getQueryBuilder().deleteAllFeatures());
    }

    /**
     * @param dataSource
     *            the dataSource to set
     */
    @Required
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
   
}
