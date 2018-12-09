package org.ff4j.jdbc.mapper;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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

import java.sql.Connection;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.feature.Feature;
import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.property.Property;
import org.ff4j.security.FF4jGrantees;
import org.ff4j.security.FF4jPermission;
import org.ff4j.utils.TimeUtils;

/**
 * Map resultset into {@link Feature}
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcFeatureMapper extends AbstractJdbcMapper implements FeatureMapper< PreparedStatement, ResultSet > {
    
    /**
     * Constructor with parameters.
     *
     * @param sqlConn
     *      connection sql
     * @param qbd
     *      query builder
     */
    public JdbcFeatureMapper(Connection sqlConn, JdbcQueryBuilder qbd) {
        super(sqlConn, qbd);
    }
    
    /** 
     * sINSERT INTO FF4J_FEATURE_PROP(UID,CLASSNAME,CREATED,LASTMODIFIED,OWNER,VAL,FIXEDVALUES,FEAT_UID)
     */
    public PreparedStatement insertFeaturePropertyStatement(Property<?> prop, String featureId) {
        PreparedStatement ps;
        try {
           
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertFeatureProperty());
            // Feature uid
            ps.setString(1, prop.getUid());
            // Classname
            ps.setString(2, prop.getType());
            // Creation Date
            ps.setTimestamp(3, TimeUtils.asSqlTimeStamp(prop.getCreationDate().get()));
            // Last Modified Date
            ps.setTimestamp(4, TimeUtils.asSqlTimeStamp(prop.getLastModifiedDate().get()));
            // Owner
            ps.setString(5, prop.getOwner().orElse(null));
            // Value
            ps.setString(6, prop.asString());
            if (prop.getFixedValues().isPresent()) {
                String fixedValues = prop.getFixedValues().get().toString();
                ps.setString(7, fixedValues.substring(1, fixedValues.length() - 1));
            } else {
                ps.setString(7, null);
            }
            ps.setString(8, featureId);
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature", sqlEx);
        }
        return ps;
    }
    
    /**
     * INSERT INTO FF4J_FEATURE_PERM(FEAT_UID,PERMISSION,USERS,ROLES)
     */
    public PreparedStatement insertFeaturePermissionStatement(String featureUID, FF4jPermission perm, FF4jGrantees acl) {
        PreparedStatement ps;
        try {
            // create statement
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertFeaturePermission());
            // Feature uid
            ps.setString(1, featureUID);
            // Classname
            ps.setString(2, perm.toString());
            // users
            ps.setString(3, String.join(",", acl.getUsers()));
            // roles
            ps.setString(4, String.join(",", acl.getRoles()));
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature permission", sqlEx);
        }
        return ps;
    }
    
    /**
     * INSERT INTO FF4J_FEATURE_STRAT(FEAT_UID,TOGGLE_CLASS)
     **/
    public PreparedStatement insertToggleStrategyStatement(String featureUID, TogglePredicate togglePredicate) {
        PreparedStatement ps;
        try {
            // create statement
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertToggleStrategy());
            // Feature uid
            ps.setString(1, featureUID);
            // Classname
            ps.setString(2, togglePredicate.getClassName());
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to toggle strategy", sqlEx);
        }
        return ps;
    }
    
    /**
     * INSERT INTO FF4J_FEATURE_STRAT_P(UID,CLASSNAME,VAL,FIXEDVALUES,STRAT_FEAT_UID,STRAT_CLASS)
     */
    public PreparedStatement insertToggleStrategyPropertyStatement(String featureUID, TogglePredicate togglePredicate, Property<?> prop) {
        PreparedStatement ps;
        try {
            // create statement
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertToggleStrategyProperties());
            // Feature uid
            ps.setString(1, prop.getUid());
            // Classname
            ps.setString(2, prop.getType());
            // Value
            ps.setString(3, prop.asString());
            if (prop.getFixedValues().isPresent()) {
                String fixedValues = prop.getFixedValues().get().toString();
                ps.setString(4, fixedValues.substring(1, fixedValues.length() - 1));
            } else {
                ps.setString(4, null);
            }
            // Feature uid
            ps.setString(5, featureUID);
            // Classname
            ps.setString(6, togglePredicate.getClassName());
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to toggle strategy", sqlEx);
        }
        return ps;
    }
    
    /** {@inheritDoc} */
    @Override
    public PreparedStatement mapToRepository(Feature feature) {
        PreparedStatement ps;
        try {
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertFeature());
            // Set the 5 entity common fields
            populateEntity(ps, feature);
            // Enable
            ps.setInt(6, feature.isEnabled() ? 1 : 0);
            // GroupName
            ps.setString(7, feature.getGroup().orElse(null));
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature", sqlEx);
        }
        return ps;
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
    @Override
    public Feature mapFromRepository(ResultSet rs) {
        try {
             Feature f = new Feature(rs.getString(FeaturesColumns.UID.colname()));
             f.setGroup(rs.getString(FeaturesColumns.GROUPNAME.colname()));
             f.setEnable(rs.getInt(FeaturesColumns.ENABLE.colname()) > 0);
             mapEntity(rs, f);
             return f;
        } catch(SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create event", sqlEx);
        }
    }

}
