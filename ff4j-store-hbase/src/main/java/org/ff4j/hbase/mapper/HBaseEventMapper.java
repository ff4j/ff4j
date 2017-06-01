package org.ff4j.hbase.mapper;

import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_ACTION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_DURATION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_KEYS;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_NAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_SOURCE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TIME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TYPE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_UID;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_USER;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_VALUE;

import org.apache.commons.lang.NotImplementedException;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.audit.Event;
import org.ff4j.core.Feature;
import org.ff4j.mapper.EventMapper;

/**
 * Mapping events into HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HBaseEventMapper implements EventMapper<Put> {

    /** {@inheritDoc} */
    @Override
    public Put toStore(Event evt) {
        Put put = new Put(Bytes.toBytes(evt.getUuid()));
        
        return put;
    }

    /** {@inheritDoc} */
    @Override
    public Event fromStore(Put bean) {
        throw new NotImplementedException("Data retrieved from HBASE are GET (not PUT)");
    }
    
    /** {@inheritDoc} */
    public Event fromStore(Result result) {
        Event evt = new Event(row.getString(COL_EVENT_SOURCE),
                row.getString(COL_EVENT_TYPE),
                row.getString(COL_EVENT_NAME),
                row.getString(COL_EVENT_ACTION));
        evt.setUuid(row.getString(COL_EVENT_UID));
        evt.setCustomKeys(row.getMap(COL_EVENT_KEYS, String.class, String.class));
        evt.setDuration(row.getLong(COL_EVENT_DURATION));
        evt.setHostName(row.getString(COL_EVENT_HOSTNAME));
        evt.setTimestamp(row.getTimestamp(COL_EVENT_TIME).getTime());
        evt.setUser(row.getString(COL_EVENT_USER));
        evt.setValue(row.getString(COL_EVENT_VALUE));
        
        return evt;
    }

}
