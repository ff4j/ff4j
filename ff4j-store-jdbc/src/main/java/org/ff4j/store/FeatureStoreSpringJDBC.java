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

import java.sql.ResultSet;
import java.sql.SQLException;
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
import org.springframework.beans.factory.annotation.Required;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.core.SingleColumnRowMapper;
import org.springframework.jdbc.core.simple.ParameterizedRowMapper;
import org.springframework.transaction.annotation.Transactional;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreSpringJDBC implements JdbcFeatureStoreConstants, FeatureStore {

    /** Row Mapper for FlipPoint. */
    private static final FlippingPointRowMapper MAPPER = new FlippingPointRowMapper();

    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;

    /**
     * Mapper from Database to FlippingPoint.
     */
    static class FlippingPointRowMapper implements ParameterizedRowMapper<Feature> {
        public FlippingPointRowMapper() {};

        @Override
        public Feature mapRow(ResultSet rs, int rowNum) throws SQLException {
            Feature f = new Feature(rs.getString("UID"), rs.getInt("ENABLE") > 0, rs.getString("DESCRIPTION"));
            f.setGroup(rs.getString(COL_FEAT_GROUPNAME));
            return f;
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        getJdbcTemplate().update(SQL_ENABLE, featId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        getJdbcTemplate().update(SQL_DISABLE, featId);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return 1 == getJdbcTemplate().queryForInt(SQL_EXIST, featId);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featId) {
        List<Feature> dbFlips = getJdbcTemplate().query(SQLQUERY_GET_FEATURE_BY_ID, MAPPER, featId);
        if (dbFlips.isEmpty()) {
            throw new FeatureNotFoundException(featId);
        }
        Feature fp = dbFlips.get(0);
        List<String> auths = getJdbcTemplate().query(SQL_GET_ROLES, new SingleColumnRowMapper<String>(), featId);
        fp.getAuthorizations().addAll(auths);
        return dbFlips.get(0);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void create(Feature fp) {
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        // Transaction wraps the method, could pipe several sql queries
        String strategyColumn = null;
        String expressionColumn = null;
        if (fp.getFlippingStrategy() != null) {
            strategyColumn = fp.getFlippingStrategy().getClass().getCanonicalName();
            expressionColumn = fp.getFlippingStrategy().getInitParams();
        }
        getJdbcTemplate().update(SQL_CREATE, fp.getUid(), fp.isEnable() ? 1 : 0, fp.getDescription(), strategyColumn,
                expressionColumn, fp.getGroup());
        if (fp.getAuthorizations() != null) {
            for (String role : fp.getAuthorizations()) {
                getJdbcTemplate().update(SQL_ADD_ROLE, fp.getUid(), role);
            }
        }
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        Feature fp = read(fpId);
        if (fp.getAuthorizations() != null) {
            for (String role : fp.getAuthorizations()) {
                getJdbcTemplate().update(SQL_DELETE_ROLE, fp.getUid(), role);
            }
        }
        getJdbcTemplate().update(SQL_DELETE, fp.getUid());
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void grantRoleOnFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        getJdbcTemplate().update(SQL_ADD_ROLE, fpId, roleName);
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void removeRoleFromFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        getJdbcTemplate().update(SQL_DELETE_ROLE, fpId, roleName);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        LinkedHashMap<String, Feature> mapFP = new LinkedHashMap<String, Feature>();
        List<Feature> lFp = getJdbcTemplate().query(SQLQUERY_ALLFEATURES, MAPPER);
        for (Feature flipPoint : lFp) {
            mapFP.put(flipPoint.getUid(), flipPoint);
        }
        return mapFP;
    }

    /** {@inheritDoc} */
    @Override
    @Transactional
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());

        // Update core Flip POINT
        String fStrategy = null;
        String fExpression = null;
        if (fp.getFlippingStrategy() != null) {
            fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
            fExpression = fp.getFlippingStrategy().getInitParams();
        }
        String enable = "0";
        if (fp.isEnable()) {
            enable = "1";
        }
        getJdbcTemplate().update(SQL_UPDATE, enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(), fp.getUid());

        // To be deleted : not in second but in first
        Set<String> toBeDeleted = new HashSet<String>();
        toBeDeleted.addAll(fpExist.getAuthorizations());
        toBeDeleted.removeAll(fp.getAuthorizations());
        for (String roleToBeDelete : toBeDeleted) {
            removeRoleFromFeature(fpExist.getUid(), roleToBeDelete);
        }

        // To be created : in second but not in first
        Set<String> toBeAdded = new HashSet<String>();
        toBeAdded.addAll(fp.getAuthorizations());
        toBeAdded.removeAll(fpExist.getAuthorizations());
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
        return "DataBaseFeatureStore [dataSource=" + dataSource + "]";
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
