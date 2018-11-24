package org.ff4j.jdbc.mapper;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.feature.togglestrategy.TogglePredicate;
import org.ff4j.jdbc.JdbcConstants.FeatureStrategyColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.ToggleStrategyMapper;
import org.ff4j.utils.JsonUtils;

/**
 * Will map a Bean into JDBC object and vice-versa.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public class JdbcFeatureToggleStrategyMapper extends AbstractJdbcMapper implements ToggleStrategyMapper< PreparedStatement, ResultSet > {

    /** Each Toggle Strategy must be link to a dedicated feature. */
    private String featureUid = null;
    
    /**
     * Constructor for the toggle strategy.
     *
     * @param sqlConn
     *      sql connection
     * @param qbd
     *      create all sql queries      
     * @param featureUid
     *      target feature
     */
    public JdbcFeatureToggleStrategyMapper(Connection sqlConn, JdbcQueryBuilder qbd, String featureUid) {
        super(sqlConn, qbd);
        this.featureUid = featureUid;
    }

    /** {@inheritDoc} */
    @Override
    public PreparedStatement mapToRepository(TogglePredicate ts) {
        PreparedStatement ps;
        try {
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertToggleStrategy());
            ps.setString(1, getFeatureUid());
            ps.setString(2, ts.getClass().getName());
            ps.setString(3, JsonUtils.mapAsJson(ts.getParams()));
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create feature", sqlEx);
        }
        return ps;
    }

    /** {@inheritDoc} */
    @Override
    public TogglePredicate mapFromRepository(ResultSet rs) {
        try {
            String uid       = rs.getString(FeatureStrategyColumns.FEATURE.colname());
            String className = rs.getString(FeatureStrategyColumns.CLASSNAME.colname());
            String iMapStr   = rs.getString(FeatureStrategyColumns.INITPARAMS.colname());
            return TogglePredicate.of(uid, className, JsonUtils.jsonAsMap(iMapStr));
       } catch(SQLException sqlEx) {
           throw new FeatureAccessException("Cannot create statement to create event", sqlEx);
       }
    }

    /**
     * Getter accessor for attribute 'featureUid'.
     *
     * @return
     *       current value of 'featureUid'
     */
    public String getFeatureUid() {
        return featureUid;
    }

}
