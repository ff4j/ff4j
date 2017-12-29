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
import java.time.ZoneId;
import java.time.ZoneOffset;

import org.ff4j.event.Event;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.jdbc.JdbcConstants.AuditTrailColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.EventMapper;
import org.ff4j.utils.JsonUtils;

/**
 * Map resultset into {@link Event}
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcEventAuditTrailMapper extends AbstractJdbcMapper implements EventMapper < PreparedStatement, ResultSet> {
  
    public JdbcEventAuditTrailMapper(Connection sqlConn, JdbcQueryBuilder qbd) {
        super(sqlConn, qbd);
    }
    
    /** {@inheritDoc} */
    @Override
    public PreparedStatement toStore(Event evt) {
        PreparedStatement stmt = null;
        try {
            stmt = sqlConn.prepareStatement(queryBuilder.sqlInsertAuditTrail());
            populateEntity(stmt, evt);
            
            stmt.setTimestamp(6, new java.sql.Timestamp(evt.getTimestamp()));
            stmt.setString( 7, evt.getScope());
            stmt.setString( 8, evt.getTargetUid());
            stmt.setString( 9, evt.getAction());
            stmt.setString(10, evt.getHostName());
            stmt.setString(11, evt.getSource());
            stmt.setLong(  12, evt.getDuration().orElse(0L));
            stmt.setString(13, evt.getValue().orElse(null));
            stmt.setString(14, evt.getCustomProperties().isPresent() ? 
                                   JsonUtils.mapAsJson(evt.getCustomProperties().get()) : null);
        } catch(SQLException sqlEx) {
            throw new AuditAccessException("Cannot create statement to create event", sqlEx);
        }
        return stmt;
    }

    /**
     * Unmarshall a resultset to Event.
     *
     * @param rs
     *      current line
     * @return
     *      bean populated
     * @throws SQLException
     *      cannot read SQL result
     */
    @Override
    public Event fromStore(ResultSet rs) {
        try {
            Event evt = new Event(rs.getString(AuditTrailColumns.UID.colname()));
            mapEntity(rs, evt);
            evt.scope(rs.getString(AuditTrailColumns.TYPE.colname()));
            evt.targetUid(rs.getString(AuditTrailColumns.NAME.colname()));
            evt.action(rs.getString(AuditTrailColumns.ACTION.colname()));
            evt.hostName(rs.getString(AuditTrailColumns.HOSTNAME.colname()));
            evt.source(rs.getString(AuditTrailColumns.SOURCE.colname()));
            evt.duration(rs.getLong(AuditTrailColumns.DURATION.colname()));
            evt.value(rs.getString(AuditTrailColumns.VALUE.colname()));
            evt.setCustomKeys(JsonUtils.jsonAsMap(rs.getString(AuditTrailColumns.KEYS.colname())));
            ZoneOffset zof = ZoneId.systemDefault().getRules().getOffset(evt.getCreationDate().get());
            evt.setTimestamp(evt.getCreationDate().get().toEpochSecond(zof));
            return evt;
        } catch(SQLException sqlEx) {
            throw new AuditAccessException("Cannot map result to Event", sqlEx);
        }
        
    }

}
