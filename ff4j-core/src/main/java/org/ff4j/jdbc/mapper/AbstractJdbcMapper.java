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
import java.sql.Timestamp;
import java.time.LocalDateTime;

import org.ff4j.FF4jEntity;
import org.ff4j.jdbc.JdbcConstants;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.utils.TimeUtils;

/**
 * Mapper for JDBC object.
 *
 * @author Cedrick LUNVEN  (@clunven)
 */
public abstract class AbstractJdbcMapper {
    
    /** sql Connection. */
    protected Connection sqlConn = null;
    
    /** helper to access JDBC queries and constants. */
    protected JdbcQueryBuilder queryBuilder = null;
    
    /**
     * Constructor with connection.
     *
     * @param sqlConn
     *      sql conenction
     * @param qbd
     *      sql query builder
     */
    public AbstractJdbcMapper(Connection sqlConn, JdbcQueryBuilder qbd) {
        this.sqlConn      = sqlConn;
        this.queryBuilder = qbd;
    }
    
    /**
     * Utility to retrieve a {@link LocalDateTime} from SQl {@link Timestamp}.
     * 
     * @param rs
     *      current resultset
     * @param colName
     *      current column name
     * @return
     *      the local time
     */
    protected LocalDateTime getLocalDateTime(ResultSet rs, String colName) {
        try {
            return TimeUtils.asLocalDateTime(rs.getTimestamp(colName));
        } catch (SQLException sqlEx) {
            throw new IllegalArgumentException("Cannot retrieve localdate time", sqlEx);
        }
    }
    
    void mapEntity(ResultSet rs, FF4jEntity<?> e) {
        try {
            e.setCreationDate(getLocalDateTime(rs, JdbcConstants.COLUMN_CREATED));
            e.setLastModified(getLocalDateTime(rs, JdbcConstants.COLUMN_LASTMODIFIED));
            e.setOwner(rs.getString(JdbcConstants.COLUMN_OWNER));
            e.setDescription(rs.getString(JdbcConstants.COLUMN_DESCRIPTION));
        } catch (SQLException sqlEx) {
            throw new IllegalArgumentException("Cannot map entity ", sqlEx);
        }
    }
    
    void populateEntity(PreparedStatement stmt, FF4jEntity<?> ent) {
        try {
            // Feature uid
            stmt.setString(1, ent.getUid());
            // Creation Date
            stmt.setTimestamp(2, TimeUtils.asSqlTimeStamp(ent.getCreationDate().get()));
            // Last Modified Date
            stmt.setTimestamp(3, TimeUtils.asSqlTimeStamp(ent.getLastModifiedDate().get()));
            // Owner
            stmt.setString(4, ent.getOwner().orElse(null));
            // Description
            stmt.setString(5, ent.getDescription().orElse(null));
        } catch (SQLException sqlEx) {
            throw new IllegalArgumentException("Cannot populate entity ", sqlEx);
        }
    }

}
