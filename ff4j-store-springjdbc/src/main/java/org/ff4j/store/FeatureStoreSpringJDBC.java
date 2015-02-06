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
import org.ff4j.store.rowmapper.FeatureRowMapper;
import org.ff4j.store.rowmapper.RoleRowMapper;
import org.ff4j.utils.ParameterUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.SingleColumnRowMapper;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.stereotype.Repository;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Repository
public class FeatureStoreSpringJDBC implements JdbcFeatureStoreConstants, FeatureStore {

    private static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";

	private static final String FEATURE_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier (param#0) cannot be null nor empty";

	/** Row Mapper for FlipPoint. */
    private static final FeatureRowMapper MAPPER = new FeatureRowMapper();

    /** SQL DataSource. */
    @Autowired
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(SQL_ENABLE, uid);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        getJdbcTemplate().update(SQL_DISABLE, uid);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        int count = getJdbcTemplate().queryForObject(SQL_EXIST, Integer.class, uid);
        return 1 == count;
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        List<Feature> dbFlips = getJdbcTemplate().query(SQLQUERY_GET_FEATURE_BY_ID, MAPPER, uid);
        if (dbFlips.isEmpty()) {
            throw new FeatureNotFoundException(uid);
        }
        Feature fp = dbFlips.get(0);
        List<String> auths = getJdbcTemplate().query(SQL_GET_ROLES, new SingleColumnRowMapper<String>(), uid);
        fp.getPermissions().addAll(auths);
        return fp;
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        // Transaction wraps the method, could pipe several sql queries
        String strategyColumn = null;
        String expressionColumn = null;
        if (fp.getFlippingStrategy() != null) {
            strategyColumn = fp.getFlippingStrategy().getClass().getCanonicalName();
            expressionColumn = ParameterUtils.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        getJdbcTemplate().update(SQL_CREATE, fp.getUid(), fp.isEnable() ? 1 : 0, fp.getDescription(), strategyColumn,
                expressionColumn, fp.getGroup(),fp.getRegionIdentifier());
        if (fp.getPermissions() != null) {
            for (String role : fp.getPermissions()) {
                getJdbcTemplate().update(SQL_ADD_ROLE_BY_GROUP_REGION, fp.getUid(), role,fp.getGroup(),fp.getRegionIdentifier());
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (!exist(uid)) {
            throw new FeatureNotFoundException(uid);
        }
        Feature fp = read(uid);
        if (fp.getPermissions() != null) {
            for (String role : fp.getPermissions()) {
                getJdbcTemplate().update(SQL_DELETE_ROLE, fp.getUid(), role);
            }
        }
        getJdbcTemplate().update(SQL_DELETE, fp.getUid());
    }

    /** {@inheritDoc} */
    @Override
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
        getJdbcTemplate().update(SQL_ADD_ROLE, uid, roleName);
    }

    /** {@inheritDoc} */
    @Override
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
        List<Feature> lFp = getJdbcTemplate().query(SQLQUERY_GET_FEATURE_GROUP, MAPPER, groupName);
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
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
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
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
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
        // ---> Feature not in Group : should not raise error
        // Feature feat = read(featureId);
        // if (feat.getGroup() != null && !feat.getGroup().equals(groupName)) {
        // throw new IllegalArgumentException("'" + featureId + "' is not in group '" + groupName + "'");
        // }
        // <----
        getJdbcTemplate().update(SQL_ADD_TO_GROUP, "", uid);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        List<Feature> lFp = getJdbcTemplate().query(SQLQUERY_ALLFEATURES, MAPPER);
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
    @Override
    @Transactional
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException(FEATURE_CANNOT_BE_NULL_NOR_EMPTY);
        }
        Feature fpExist = read(fp.getUid());

        // Update core Flip POINT
        String fStrategy = null;
        String fExpression = null;
        if (fp.getFlippingStrategy() != null) {
            fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
            fExpression = ParameterUtils.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        String enable = "0";
        if (fp.isEnable()) {
            enable = "1";
        }
        getJdbcTemplate().update(SQL_UPDATE, enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(), fp.getUid());

        // To be deleted : not in second but in first
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

        // enable/disable
        if (fp.isEnable() != fpExist.isEnable()) {
            if (fp.isEnable()) {
                enable(fp.getUid());
            } else {
                disable(fp.getUid());
            }
        }
    }
       
    
 // Logic to fetch feature based on group and region
    
    /**
     * Enable features based by group name and region
     * @param featId
     * @param groupName
     * @param region
     */
    public void enable(String featId,String groupName,String region) {
        if (!exist(featId,groupName,region)) {
            throw new FeatureNotFoundException(featId);
        }
        getJdbcTemplate().update(SQL_FEATURE_STATUS_ENABLE, featId,groupName,region);
    }

    
    public void disable(String featId,String groupName,String region) {
        if (!exist(featId,groupName,region)) {
            throw new FeatureNotFoundException(featId);
        }
        getJdbcTemplate().update(SQL_FEATURE_STATUS_DISABLE, featId,groupName,region);
    }

    public boolean exist(String featId,String groupName,String region) {
        return 1 == getJdbcTemplate().queryForInt(SQL_EXIST_FOR_GROUP_REGION, featId,groupName,region);
    }

    public Feature read(String featId,String groupName,String region) {
        List<Feature> dbFlips = getJdbcTemplate().query(SQLQUERY_GET_FEATURE_BY_GROUP_REGION, MAPPER, featId,groupName,region);
        if (dbFlips.isEmpty()) {
            throw new FeatureNotFoundException(featId);
        }
        Feature fp = dbFlips.get(0);
        List<String> auths = getJdbcTemplate().query(SQL_GET_ROLES_BY_GROUP_REGION, new SingleColumnRowMapper<String>(), featId,groupName,region);
        fp.getPermissions().addAll(auths);
        return dbFlips.get(0);
    }

    public void delete(String fpId,String groupName,String region) {
        if (!exist(fpId,groupName,region)) {
            throw new FeatureNotFoundException(fpId);
        }
        Feature fp = read(fpId,groupName,region);
        if (fp.getPermissions() != null) {
            for (String role : fp.getPermissions()) {
                getJdbcTemplate().update(SQL_DELETE_ROLE_BY_GROUP_REGION, fp.getUid(), role,groupName,region);
            }
        }
        getJdbcTemplate().update(SQL_DELETE_BY_GROUP_REGION, fp.getUid(),groupName,region);
    }
    
    public List<String> readAllGroups(String region){
    	
		return getJdbcTemplate().query(SQL_GET_ALLGROUPS_BY_REGION, new SingleColumnRowMapper<String>(), region);

    }
    
    public void grantRoleOnFeature(String fpId, String roleName,String groupName,String region) {
        if (!exist(fpId,groupName,region)) {
            throw new FeatureNotFoundException(fpId);
        }
        getJdbcTemplate().update(SQL_ADD_ROLE_BY_GROUP_REGION, fpId, roleName,groupName,region);
    }

    public void removeRoleFromFeature(String fpId, String roleName,String groupName,String region) {
        if (!exist(fpId,groupName,region)) {
            throw new FeatureNotFoundException(fpId);
        }
        getJdbcTemplate().update(SQL_DELETE_ROLE_BY_GROUP_REGION, fpId, roleName);
    }

    public Map<String, Feature> readAll(String groupName,String region) {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        
        List<Feature> lFp = null;
        if(groupName ==null){
        	
        	lFp = getJdbcTemplate().query(SQLQUERY_ALLFEATURES_BY_REGION, MAPPER,region);
        	
        	 for (Feature flipPoint : lFp) {
                 mapFP.put(generateAFeatureKey(flipPoint.getUid(), flipPoint.getGroup()), flipPoint);
             }
        	
        }else{
        	
        	lFp = getJdbcTemplate().query(SQLQUERY_ALLFEATURES_BY_GROUP_REGION, MAPPER,groupName,region);
        	 
        	for (Feature flipPoint : lFp) {
                 mapFP.put(flipPoint.getUid(), flipPoint);
             }
        }
        
       
        return mapFP;
    }

    public void updateByGroupRegion(Feature fp) {
        Feature fpExist = read(fp.getUid(),fp.getGroup(),fp.getRegionIdentifier());

        // Update core Flip POINT
        String fStrategy = null;
        String fExpression = null;
        if (fp.getFlippingStrategy() != null) {
            fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
            fExpression = ParameterUtils.fromMap(fp.getFlippingStrategy().getInitParams());
        }
        String enable = "0";
        if (fp.isEnable()) {
            enable = "1";
        }
        getJdbcTemplate().update(SQL_UPDATE_BY_GROUP_REGION, enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(),fp.getRegionIdentifier(), fp.getUid(),fp.getGroup(),fp.getRegionIdentifier());

        // To be deleted : not in second but in first
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

        // enable/disable
        if (fp.isEnable() != fpExist.isEnable()) {
            if (fp.isEnable()) {
                enable(fp.getUid(),fp.getGroup(),fp.getRegionIdentifier());
            } else {
                disable(fp.getUid(),fp.getGroup(),fp.getRegionIdentifier());
            }
        }
    }


    /**
     * @param dataSource
     *            the dataSource to set
     */
    @Required
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
        if(this.dataSource != null){
        	
        	sb.append("\"datasource\":\"" + this.dataSource.getClass() + "\"");	
        } else{
        	
        	sb.append("\"datasource\":\"" + this.dataSource + "\"");
        }        Set<String> myFeatures = readAll().keySet();
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
     * Generates a key by combining the passed feature name and group name strings.
     * @param featureName feature name
     * @param groupName group name
     * @return
     */
    private String generateAFeatureKey(String featureName, String groupName) {
		
    	return featureName+groupName;
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

}