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

    /**
     * Default Constructor.
     */
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
            throw new RuntimeException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            releaseConnection(rs, ps);
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
                f = new Feature(rs.getString("UID"), rs.getInt("ENABLE") > 0, rs.getString("DESCRIPTION"));
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
            throw new RuntimeException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            releaseConnection(rs, ps);
        }
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
            ps.setString(1, fp.getUid());
            ps.setInt(2, fp.isEnable() ? 1 : 0);
            ps.setString(3, fp.getDescription());
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
            try {
                if (!sqlConn.isClosed()) {
                    sqlConn.rollback();
                }
            } catch (SQLException e) {}
            throw new IllegalArgumentException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            releaseConnection(null, ps);
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
            try {
                if (!sqlConn.isClosed()) {
                    sqlConn.rollback();
                }
            } catch (SQLException e) {}
            throw new IllegalArgumentException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            releaseConnection(null, ps);
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
     * Properly close working SQL Object.
     * 
     * @param rs
     *            result set if provided
     * @param ps
     *            prepared statement if provided
     */
    private void releaseConnection(ResultSet rs, PreparedStatement ps) {
        // Close resulset
        try {
            if (rs != null) {
                rs.close();
            }
        } catch (SQLException e) {}
        // Close connection
        try {
            if (ps != null && ps.getConnection() != null) {
                ps.getConnection().close();
            }
        } catch (SQLException e) {}
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
    private PreparedStatement buildStatement(String query, String... params) throws SQLException {
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
            throw new IllegalArgumentException("Cannot update features database, SQL ERROR", sqlEX);
        } finally {
            releaseConnection(null, ps);
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
                Feature f = new Feature(rs.getString("UID"), rs.getInt("ENABLE") > 0, rs.getString("DESCRIPTION"));
                mapFP.put(f.getUid(), f);
            }

            // Returns Roles
            rs = ps.getConnection().prepareStatement(SQL_GET_ALLROLES).executeQuery();
            while (rs.next()) {
                String uid = rs.getString("FEAT_UID");
                mapFP.get(uid).getAuthorizations().add(rs.getString("ROLE_NAME"));
            }
            return mapFP;

        } catch (SQLException sqlEX) {
            throw new RuntimeException("Cannot check feature existence, error related to database", sqlEX);
        } finally {
            releaseConnection(rs, ps);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Feature fpExist = read(fp.getUid());

        // Update core Flip POINT
        update(SQL_UPDATE, fp.getDescription(), fp.getUid());

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
