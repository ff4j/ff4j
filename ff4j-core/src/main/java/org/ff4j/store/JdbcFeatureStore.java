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
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;

/**
 * Implementation of {@link FeatureStore} to work with RDBMS through JDBC.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureStore implements JdbcFeatureStoreConstants, FeatureStore {

    /** Access to storage. */
    private DataSource dataSource;

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

    /** {@inheritDoc} */
    @Override
    public void enable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        update(SQL_ENABLE, featId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featId) {
        if (!exist(featId)) {
            throw new FeatureNotFoundException(featId);
        }
        update(SQL_DISABLE, featId);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            ps = buildStatement(SQL_EXIST, featId);
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
    public Feature read(String featId) {
        PreparedStatement ps = null;
        ResultSet rs = null;
        try {
            // Returns features
            ps = buildStatement(SQLQUERY_GET_FEATURE_BY_ID, featId);
            rs = ps.executeQuery();
            Feature f = null;
            if (rs.next()) {
                f = mapRow2Feature(rs);
            } else {
                throw new FeatureNotFoundException(featId);
            }

            // 2nd request
            ps = ps.getConnection().prepareStatement(SQL_GET_ROLES);
            ps.setString(1, featId);
            rs = ps.executeQuery();
            while (rs.next()) {
                f.getAuthorizations().add(rs.getString("ROLE_NAME"));
            }
            return f;
        } catch (SQLException sqlEX) {
            throw new FeatureAccessException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            closeResultSet(rs);
            closeStatement(ps);
        }
    }

    /**
     * Map feature result to bean.
     * 
     * @param rs
     *            current resultSet
     * @return current Feature without roles
     * @throws SQLException
     *             error accured when parsing resultSet
     */
    private Feature mapRow2Feature(ResultSet rs) throws SQLException {
        // Feature
        Feature f = null;
        boolean enabled = rs.getInt(COL_FEAT_ENABLE) > 0;
        String featUid = rs.getString(COL_FEAT_UID);
        f = new Feature(featUid, enabled, rs.getString(COL_FEAT_DESCRIPTION), rs.getString(COL_FEAT_GROUPNAME));
        // Strategy
        String strategy = rs.getString(COL_FEAT_STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            try {
                FlippingStrategy flipStrategy = (FlippingStrategy) Class.forName(strategy).newInstance();
                String expr = rs.getString(COL_FEAT_EXPRESSION);
                flipStrategy.init(featUid, expr);
                f.setFlippingStrategy(flipStrategy);
            } catch (InstantiationException ie) {
                throw new FeatureAccessException("Cannot instanciate Strategy, no default contructor available", ie);
            } catch (IllegalAccessException iae) {
                throw new FeatureAccessException("Cannot instanciate Strategy, no visible constructor", iae);
            } catch (ClassNotFoundException e) {
                throw new FeatureAccessException("Cannot instanciate Strategy, classNotFound", e);
            }
        }
        return f;
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
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
                expressionColumn = fp.getFlippingStrategy().getInitParams();
            }
            ps.setString(idx++, strategyColumn);
            ps.setString(idx++, expressionColumn);
            ps.setString(idx++, fp.getGroup());
            ps.executeUpdate();

            // Create roles
            if (fp.getAuthorizations() != null) {
                for (String role : fp.getAuthorizations()) {
                    ps = sqlConn.prepareStatement(SQL_ADD_ROLE);
                    ps.setString(1, fp.getUid());
                    ps.setString(2, role);
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
    @Override
    public void delete(String fpId) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        Connection sqlConn = null;
        PreparedStatement ps = null;
        try {
            Feature fp = read(fpId);

            // Create connection
            sqlConn = getDataSource().getConnection();
            sqlConn.setAutoCommit(false);

            // Delete Roles
            if (fp.getAuthorizations() != null) {
                for (String role : fp.getAuthorizations()) {
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
    public void grantRoleOnFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        update(SQL_ADD_ROLE, fpId, roleName);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String fpId, String roleName) {
        if (!exist(fpId)) {
            throw new FeatureNotFoundException(fpId);
        }
        update(SQL_DELETE_ROLE, fpId, roleName);
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
                Feature f = mapRow2Feature(rs);
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(SQL_GET_ALLROLES).executeQuery();
            while (rs.next()) {
                String uid = rs.getString(COL_ROLE_FEATID);
                // mapFP.get(uid).getAuthorizations() cannot be null thanks to FOREIGN KEY
                mapFP.get(uid).getAuthorizations().add(rs.getString(COL_ROLE_ROLENAME));
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
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());
        // Update core Flip POINT
        String enable = "0";
        if (fp.isEnable()) {
            enable = "1";
        }
        String fStrategy = null;
        String fExpression = null;
        if (fp.getFlippingStrategy() != null) {
            fStrategy = fp.getFlippingStrategy().getClass().getCanonicalName();
            fExpression = fp.getFlippingStrategy().getInitParams();
        }
        update(SQL_UPDATE, enable, fp.getDescription(), fStrategy, fExpression, fp.getGroup(), fp.getUid());

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
    }

    /**
     * Getter accessor for attribute 'dataSource'.
     * 
     * @return current value of 'dataSource'
     */
    public DataSource getDataSource() {
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
