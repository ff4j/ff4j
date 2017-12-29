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

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.feature.Feature;
import org.ff4j.jdbc.JdbcConstants.FeaturesColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.FeatureMapper;

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
    
    /** {@inheritDoc} */
    @Override
    public PreparedStatement toStore(Feature feature) {
        PreparedStatement ps;
        try {
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertFeature());
            // Set the 5 entity common fields
            populateEntity(ps, feature);
            // Enable
            ps.setInt(6, feature.isEnable() ? 1 : 0);
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
    public Feature fromStore(ResultSet rs) {
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
