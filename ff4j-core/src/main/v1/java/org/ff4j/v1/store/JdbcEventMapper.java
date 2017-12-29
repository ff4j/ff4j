package org.ff4j.v1.store;

import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_ACTION;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_DURATION;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_KEYS;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_NAME;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_SOURCE;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_TIME;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_TYPE;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_USER;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_UUID;
import static org.ff4j.v1.store.JdbcStoreConstants.COL_EVENT_VALUE;

import java.sql.ResultSet;
import java.sql.SQLException;

import org.ff4j.v1.audit.Event;
import org.ff4j.v1.utils.MappingUtil;

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
