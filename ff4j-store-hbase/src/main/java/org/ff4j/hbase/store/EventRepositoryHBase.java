package org.ff4j.hbase.store;

import static org.ff4j.hbase.HBaseConstants.AUDIT_TABLENAME;
import static org.ff4j.hbase.HBaseConstants.AUDIT_CF;
import static org.ff4j.hbase.HBaseConstants.AUDIT_TABLENAME_ID;

import java.io.IOException;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import org.apache.hadoop.hbase.client.Connection;
import org.apache.hadoop.hbase.client.ConnectionFactory;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Table;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.hbase.HBaseConnection;
import org.ff4j.hbase.mapper.HBaseEventMapper;
import org.ff4j.utils.Util;

/**
 * Implementation of audit HBASE.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryHBase extends AbstractEventRepository {
    
    /** Mapper. */
    private static final HBaseEventMapper MAPPER = new HBaseEventMapper();
    
    /** Connection to store Cassandra. */
    private HBaseConnection conn;
    
    /**
     * Default constructor.
     */
    public EventRepositoryHBase() {
    }
    
    /**
     * Initialization through {@link HBaseConnection}.
     *
     * @param conn
     *      current client to cassandra db
     */
    public EventRepositoryHBase(HBaseConnection conn) {
        this.conn = conn;
    }
    
    /**
     * Utility to insert data into events.
     *
     * @param putQuery
     *      query to update DB.
     */
    private void executePutCommand(Put putQuery) {
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                table.put(putQuery);
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot execute command", e);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        conn.createTable(AUDIT_TABLENAME_ID, Util.set(AUDIT_CF));
    }
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event evt) {
        Util.assertEvent(evt);
        executePutCommand(MAPPER.toStore(evt));
        return true;
    }

    /** {@inheritDoc} */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
    }

    /**
     * Getter accessor for attribute 'conn'.
     *
     * @return
     *       current value of 'conn'
     */
    public HBaseConnection getConn() {
        return conn;
    }

    /**
     * Setter accessor for attribute 'conn'.
     *
     * @param conn
     *      new value for 'conn '
     */
    public void setConn(HBaseConnection conn) {
        this.conn = conn;
    }
    
}
