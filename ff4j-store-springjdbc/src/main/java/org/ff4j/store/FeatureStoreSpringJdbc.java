package org.ff4j.store;

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
import org.ff4j.property.AbstractProperty;
import org.ff4j.store.rowmapper.CustomPropertyRowMapper;
import org.ff4j.store.rowmapper.FeatureRowMapper;
import org.ff4j.store.rowmapper.RoleRowMapper;
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
public class FeatureStoreSpringJdbc extends AbstractFeatureStore implements JdbcStoreConstants {

    /** Row Mapper for FlipPoint. */
    private static final FeatureRowMapper FMAPPER = new FeatureRowMapper();

    /** Mapper for custom properties. */
    private static final CustomPropertyRowMapper PMAPPER = new CustomPropertyRowMapper();

    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;

    /** {@inheritDoc} */
    public void enable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(SQL_ENABLE, uid);
    }

    /** {@inheritDoc} */
    public void disable(String uid) {
        Util.assertHasLength(uid);
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(SQL_DISABLE, uid);
    }

    /** {@inheritDoc} */
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        return 1 == getJdbcTemplate().queryForObject(SQL_EXIST, Integer.class, uid);
    }

    /** {@inheritDoc} */
    public Feature read(String uid) {
        Util.assertHasLength(uid);
        try {
            Feature feature = getJdbcTemplate().queryForObject(SQL_GETFEATUREBYID, FMAPPER, uid);
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
        List<AbstractProperty<?>> listOfProps = getJdbcTemplate().query(SQL_GETREFPROPERTIESBYID, PMAPPER, fp.getUid());
        for (AbstractProperty<?> ap : listOfProps) {
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
                getJdbcTemplate().query(SQL_GET_ROLES, 
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
            strategyColumn = fp.getFlippingStrategy().getClass().getCanonicalName();
            expressionColumn = MappingUtil.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        getJdbcTemplate().update(SQL_CREATE, 
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
            getJdbcTemplate().update(SQL_DELETE_ROLES, fp.getUid());
            for (String role : fp.getPermissions()) {
                getJdbcTemplate().update(SQL_ADD_ROLE, fp.getUid(), role);
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
            getJdbcTemplate().update(SQL_DELETE_CUSTOMPROPERTIES, fp.getUid());
            for (String propertyName : fp.getCustomProperties().keySet()) {
                AbstractProperty<?> ap = fp.getCustomProperties().get(propertyName);
                String fixedValues = null;
                if (ap.getFixedValues() != null && ap.getFixedValues().size() > 0) {
                    fixedValues = ap.getFixedValues().toString();
                    fixedValues = fixedValues.substring(1, fixedValues.length() - 1);
                }
                getJdbcTemplate().update(SQL_CREATE_CUSTOMPROPERTY, 
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
        getJdbcTemplate().update(SQL_DELETE_ROLES, featureId);
    }
    
    /**
     * Delete properties related to a feature.
     *
     * @param featureId
     *      current feature uid
     */
    private void deleteProperties(String featureId) {
        getJdbcTemplate().update(SQL_DELETE_CUSTOMPROPERTIES, featureId);
    }
    
    /**
     * Delete core feature.
     *
     * @param featureId
     *      current feature uid
     */
    private void deleteCoreFeature(String featureId) {
        getJdbcTemplate().update(SQL_DELETE, featureId);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
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
        getJdbcTemplate().update(SQL_ADD_ROLE, uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
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
        getJdbcTemplate().update(SQL_DELETE_ROLE, uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        getJdbcTemplate().update(SQL_ENABLE_GROUP, groupName);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (!existGroup(groupName)) {
            throw new GroupNotFoundException(groupName);
        }
        getJdbcTemplate().update(SQL_DISABLE_GROUP, groupName);
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        int count = getJdbcTemplate().queryForObject(SQL_EXIST_GROUP, Integer.class, groupName);
        return count > 0;
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
        List<Feature> lFp = getJdbcTemplate().query(SQLQUERY_GET_FEATURE_GROUP, FMAPPER, groupName);
        for (Feature flipPoint : lFp) {
            mapFP.put(flipPoint.getUid(), flipPoint);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
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
        getJdbcTemplate().update(SQL_ADD_TO_GROUP, groupName, uid);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
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
        getJdbcTemplate().update(SQL_ADD_TO_GROUP, "", uid);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        List<Feature> lFp = getJdbcTemplate().query(SQLQUERY_ALLFEATURES, FMAPPER);
        for (Feature flipPoint : lFp) {
            mapFP.put(flipPoint.getUid(), flipPoint);
        }
        // Populating Roles
        RoleRowMapper rrm = new RoleRowMapper();
        getJdbcTemplate().query(SQL_GET_ALLROLES, rrm);
        Map<String, Set<String>> roles = rrm.getRoles();
        for (String featId : roles.keySet()) {
            if (mapFP.containsKey(featId)) {
                mapFP.get(featId).getPermissions().addAll(roles.get(featId));
            }
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> setOfGroup = new HashSet<String>();
        setOfGroup.addAll(getJdbcTemplate().query(SQLQUERY_ALLGROUPS, new SingleColumnRowMapper<String>()));
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
    @Transactional
    public void clear() {
        getJdbcTemplate().update(SQL_DELETE_ALL_ROLES);
        getJdbcTemplate().update(SQL_DELETE_ALL_CUSTOMPROPERTIES);
        getJdbcTemplate().update(SQL_DELETE_ALL_FEATURES);
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
   
}
