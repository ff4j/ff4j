package org.ff4j.store;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.core.Feature;
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
public class DataBaseFeatureStore implements FeatureStore {

    /** sql query expression */
    private static final String SQLQUERY_ALLFEATURES = "SELECT UID,ENABLE,DESCRIPTION FROM FF4J_FEATURES";

    /** sql query expression */
    private static final String SQLQUERY_GET_FEATURE_BY_ID = "SELECT UID,ENABLE,DESCRIPTION FROM FF4J_FEATURES WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_EXIST = "SELECT COUNT(UID) FROM FF4J_FEATURES WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_DISABLE = "UPDATE FF4J_FEATURES SET ENABLE = 0 WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_ENABLE = "UPDATE FF4J_FEATURES SET ENABLE = 1 WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_CREATE = "INSERT INTO FF4J_FEATURES(UID, ENABLE, DESCRIPTION) VALUES(?, ?, ?)";

    /** sql query expression */
    private static final String SQL_DELETE = "DELETE FROM FF4J_FEATURES WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_UPDATE = "UPDATE FF4J_FEATURES SET DESCRIPTION = ? WHERE UID = ?";

    /** sql query expression */
    private static final String SQL_ADD_ROLE = "INSERT INTO FF4J_ROLES(FEAT_UID, ROLE_NAME) VALUES (?,?)";

    /** sql query expression */
    private static final String SQL_DELETE_ROLE = "DELETE FROM FF4J_ROLES WHERE FEAT_UID = ? AND ROLE_NAME = ?";

    /** sql query expression */
    private static final String SQL_GET_FPOINT_USRROLE = "SELECT ROLE_NAME FROM FF4J_ROLES WHERE FEAT_UID = ?";

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
            return new Feature(rs.getString("UID"), rs.getInt("ENABLE") > 0, rs.getString("DESCRIPTION"));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featId) {
        getJdbcTemplate().update(SQL_ENABLE, featId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featId) {
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
        List<String> auths = getJdbcTemplate().query(SQL_GET_FPOINT_USRROLE, new SingleColumnRowMapper<String>(), featId);
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
        getJdbcTemplate().update(SQL_CREATE, fp.getUid(), fp.isEnable() ? 1 : 0, fp.getDescription());
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
        getJdbcTemplate().update(SQL_UPDATE, fp.getDescription(), fp.getUid());

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
