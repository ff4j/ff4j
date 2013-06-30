package org.ff4j.store;

import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.Feature;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.springframework.beans.factory.InitializingBean;
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
public class DataBaseFeatureStore implements FeatureStore, InitializingBean {

	/** sql query expression */
	private static final String SQL_FPOINT_ALL	= "SELECT UID,ENABLE,DESCRIPTION FROM FLIP_POINT";
	
	/** sql query expression */
	private static final String SQL_FPOINT		= "SELECT UID,ENABLE,DESCRIPTION FROM FLIP_POINT WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_EXIST		= "SELECT COUNT(UID) FROM FLIP_POINT WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_DISABLE 	= "UPDATE FLIP_POINT SET ENABLE = 0 WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_ENABLE 		= "UPDATE FLIP_POINT SET ENABLE = 1 WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_CREATE   	= "INSERT INTO FLIP_POINT(UID, ENABLE, DESCRIPTION) VALUES(?, ?, ?)";
	
	/** sql query expression */
	private static final String SQL_DELETE   	= "DELETE FROM FLIP_POINT WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_UPDATE      = "UPDATE FLIP_POINT SET DESCRIPTION = ? WHERE UID = ?";
	
	/** sql query expression */
	private static final String SQL_ADD_ROLE    = "INSERT INTO FLIP_POINT_ROLE(FPOINT_UID, ROLE_NAME) VALUES (?,?)";
	
	/** sql query expression */
	private static final String SQL_DELETE_ROLE = "DELETE FROM FLIP_POINT_ROLE WHERE FPOINT_UID = ? AND ROLE_NAME = ?";
	
	/** sql query expression */
	private static final String SQL_GET_FPOINT_USRROLE  = "SELECT ROLE_NAME FROM FLIP_POINT_ROLE WHERE FPOINT_UID = ?";
	
	/** Row Mapper for FlipPoint. */
	private static FlippingPointRowMapper FPOINT_MAPPER  = new FlippingPointRowMapper();
	
	/** SQL DataSource. */
	private DataSource dataSource;
	
	/** Access to storage. */
	private JdbcTemplate jdbcTemplate;
	
	/**
	 * Mapper from Database to FlippingPoint.
	 */
	static class FlippingPointRowMapper implements ParameterizedRowMapper<Feature> {
		public FlippingPointRowMapper(){};
		public Feature mapRow(ResultSet rs, int rowNum) throws SQLException {
			return new Feature(rs.getString("UID"), rs.getInt("ENABLE") > 0, rs.getString("DESCRIPTION"));
		}
	}

	/** {@inheritDoc} */
	public void afterPropertiesSet() throws Exception {
		this.jdbcTemplate = new JdbcTemplate(dataSource);
	}	

	/** {@inheritDoc} */
	public void enable(String featId) {
		jdbcTemplate.update(SQL_ENABLE, featId);
	}

	/** {@inheritDoc} */
	public void disable(String featId) {
		jdbcTemplate.update(SQL_DISABLE, featId);
	}

	/** {@inheritDoc} */
	public boolean exist(String featId) { 
		return 1 == jdbcTemplate.queryForInt(SQL_EXIST, featId);
	}

	/** {@inheritDoc} */
	public Feature read(String featId)
	throws FeatureNotFoundException {
		List<Feature> dbFlips = jdbcTemplate.query(SQL_FPOINT, FPOINT_MAPPER, featId);
		if (dbFlips.isEmpty()) throw new FeatureNotFoundException(featId);
		Feature fp = dbFlips.get(0);
		List < String > auths = jdbcTemplate.query(SQL_GET_FPOINT_USRROLE, new SingleColumnRowMapper<String>(), featId);
		fp.getAuthorizations().addAll(auths);
		return dbFlips.get(0);
	}
	
	/** {@inheritDoc} */
	@Transactional
	public void create(Feature fp) {
		if (exist(fp.getUid())) {
			throw new FeatureAlreadyExistException(fp.getUid());
		}
		// Transaction wraps the method, could pipe several sql queries
		jdbcTemplate.update(SQL_CREATE, fp.getUid(), fp.isEnable() ? 1 : 0, fp.getDescription());
		if (fp.getAuthorizations() != null ) {
			for (String role : fp.getAuthorizations()) {
				jdbcTemplate.update(SQL_ADD_ROLE, fp.getUid(), role);
			}
		}
	}

	/** {@inheritDoc} */
	@Transactional
	public void delete(String fpId) {
		if (!exist(fpId)) {
			throw new FeatureNotFoundException(fpId);
		}
		Feature fp = read(fpId);
		if (fp.getAuthorizations() != null ) {
			for (String role : fp.getAuthorizations()) {
				jdbcTemplate.update(SQL_DELETE_ROLE, fp.getUid(), role);
			}
		}
		jdbcTemplate.update(SQL_DELETE, fp.getUid());
	}

	/** {@inheritDoc} */
	@Transactional
	public void grantRoleOnFeature(String fpId, String roleName) {
		if (!exist(fpId)) {
			throw new FeatureNotFoundException(fpId);
		}
		jdbcTemplate.update(SQL_ADD_ROLE, fpId, roleName);
	}

	/** {@inheritDoc} */
	@Transactional
	public void removeRoleFromFeature(String fpId, String roleName) {
		if (!exist(fpId)) {
			throw new FeatureNotFoundException(fpId);
		}
		jdbcTemplate.update(SQL_DELETE_ROLE, fpId, roleName);
	}

	/** {@inheritDoc} */
	public LinkedHashMap< String, Feature> readAll() {
		LinkedHashMap < String, Feature> mapFP = new LinkedHashMap<String, Feature>();
		List<Feature> lFp = jdbcTemplate.query(SQL_FPOINT_ALL, FPOINT_MAPPER);
		for (Feature flipPoint : lFp) {
			mapFP.put(flipPoint.getUid(), flipPoint);
		}
		return mapFP;
	}
	
	/** {@inheritDoc} */
	@Transactional
	public void update(Feature fp) {
		Feature fpExist = read(fp.getUid());
		
		// Update core Flip POINT
		jdbcTemplate.update(SQL_UPDATE, fp.getDescription(), fp.getUid());
		
		// To be deleted : not in second but in first
		Set < String > toBeDeleted = new HashSet<String>();
		toBeDeleted.addAll(fpExist.getAuthorizations());
		toBeDeleted.removeAll(fp.getAuthorizations());
		for (String roleToBeDelete : toBeDeleted) {
			removeRoleFromFeature(fpExist.getUid(), roleToBeDelete);
		}
		
		// To be created : in second but not in first
		Set < String > toBeAdded = new HashSet<String>();
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
	 * @param dataSource the dataSource to set
	 */
	@Required
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

}
