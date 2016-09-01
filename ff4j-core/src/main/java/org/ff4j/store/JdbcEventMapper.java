package org.ff4j.store;

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

import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_ACTION;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_DURATION;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_KEYS;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_NAME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_SOURCE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_TIME;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_TYPE;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_USER;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_UUID;
import static org.ff4j.store.JdbcStoreConstants.COL_EVENT_VALUE;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.audit.Event;
import org.ff4j.utils.MappingUtil;

/**
 * Map resultset into {@link Event}
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcEventMapper {
    
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
    public Event mapEvent(ResultSet rs) throws SQLException {
        // Feature
        Event evt = new Event();
        evt.setUuid(rs.getString(COL_EVENT_UUID));
        evt.setTimestamp(rs.getTimestamp(COL_EVENT_TIME).getTime());
        evt.setType(rs.getString(COL_EVENT_TYPE));
        evt.setName(rs.getString(COL_EVENT_NAME));
        evt.setAction(rs.getString(COL_EVENT_ACTION));
        evt.setHostName(rs.getString(COL_EVENT_HOSTNAME));
        evt.setSource(rs.getString(COL_EVENT_SOURCE));
        evt.setDuration(rs.getLong(COL_EVENT_DURATION));
        evt.setUser(rs.getString(COL_EVENT_USER));
        evt.setValue(rs.getString(COL_EVENT_VALUE));
        evt.setCustomKeys(MappingUtil.toMap(rs.getString(COL_EVENT_KEYS)));
        return evt;
    }

}
