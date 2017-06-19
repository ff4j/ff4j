package org.ff4j.hbase.store;

/*
 * #%L
 * ff4j-store-hbase
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


import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_DISCONNECT;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.hbase.HBaseConstants.AUDIT_CF;
import static org.ff4j.hbase.HBaseConstants.AUDIT_TABLENAME;
import static org.ff4j.hbase.HBaseConstants.AUDIT_TABLENAME_ID;
import static org.ff4j.hbase.HBaseConstants.B_AUDIT_CF;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_ACTION;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_HOSTNAME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_NAME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_SOURCE;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_TIME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_TYPE;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_UID;
import static org.ff4j.hbase.HBaseConstants.COLS_EVENT;
import static org.ff4j.hbase.HBaseConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.hbase.HBaseConstants.COL_EVENT_NAME;
import static org.ff4j.hbase.HBaseConstants.COL_EVENT_SOURCE;
import static org.ff4j.hbase.HBaseConstants.COL_EVENT_USER;
import static org.ff4j.hbase.HBaseConstants.COL_EVENT_UID;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.apache.hadoop.hbase.client.Connection;
import org.apache.hadoop.hbase.client.ConnectionFactory;
import org.apache.hadoop.hbase.client.Delete;
import org.apache.hadoop.hbase.client.Get;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.client.ResultScanner;
import org.apache.hadoop.hbase.client.Scan;
import org.apache.hadoop.hbase.client.Table;
import org.apache.hadoop.hbase.filter.CompareFilter.CompareOp;
import org.apache.hadoop.hbase.filter.Filter;
import org.apache.hadoop.hbase.filter.FilterList;
import org.apache.hadoop.hbase.filter.SingleColumnValueFilter;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
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
    
    private Scan buildQuery(EventQueryDefinition qDef, Set< String > columnNames, String type) {
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                
                // Columns to retrieve dynamically
                Scan s = new Scan();
                for (String col : columnNames) {
                    s.addColumn(B_AUDIT_CF, Bytes.toBytes(col));
                }
                
                /*
                s.addColumn(B_AUDIT_CF, B_EVENT_UID);
                s.addColumn(B_AUDIT_CF, B_EVENT_SOURCE);
                s.addColumn(B_AUDIT_CF, B_EVENT_NAME);
                s.addColumn(B_AUDIT_CF, B_EVENT_ACTION);
                s.addColumn(B_AUDIT_CF, B_EVENT_TYPE);
                s.addColumn(B_AUDIT_CF, B_EVENT_DURATION);
                s.addColumn(B_AUDIT_CF, B_EVENT_HOSTNAME);
                s.addColumn(B_AUDIT_CF, B_EVENT_USER);
                s.addColumn(B_AUDIT_CF, B_EVENT_VALUE);
                s.addColumn(B_AUDIT_CF, B_EVENT_DATE);
                s.addColumn(B_AUDIT_CF, B_EVENT_TIME);
                s.addColumn(B_AUDIT_CF, B_EVENT_KEYS);
                */
               
                // Filters
                FilterList filterList = new FilterList(FilterList.Operator.MUST_PASS_ALL);

                // from
                SingleColumnValueFilter filterFrom = 
                        new SingleColumnValueFilter(B_AUDIT_CF, B_EVENT_TIME, 
                                CompareOp.GREATER_OR_EQUAL, Bytes.toBytes(qDef.getFrom()));
                filterFrom.setFilterIfMissing(true);
                filterList.addFilter(filterFrom);
                
                // To
                SingleColumnValueFilter filterTo =
                        new SingleColumnValueFilter(B_AUDIT_CF, B_EVENT_TIME, 
                                CompareOp.LESS_OR_EQUAL, Bytes.toBytes(qDef.getTo()));
                filterTo.setFilterIfMissing(true);
                filterList.addFilter(filterTo);
                
                // Type
                if (null != type) {
                    filterList.addFilter(
                            new SingleColumnValueFilter(B_AUDIT_CF, B_EVENT_TYPE, CompareOp.EQUAL, Bytes.toBytes(type)));
                }
                
                // Actions
                if (!qDef.getActionFilters().isEmpty()) {
                    filterList.addFilter(
                            createFilterListOR(B_EVENT_ACTION, qDef.getActionFilters()));
                }
                // Host
                if (!qDef.getHostFilters().isEmpty()) {
                    filterList.addFilter(
                            createFilterListOR(B_EVENT_HOSTNAME, qDef.getHostFilters()));
                }
                // Names
                if (!qDef.getNamesFilter().isEmpty()) {
                    filterList.addFilter(
                            createFilterListOR(B_EVENT_NAME, qDef.getNamesFilter()));
                }
                // Sources
                if (!qDef.getSourceFilters().isEmpty()) {
                    filterList.addFilter(
                            createFilterListOR(B_EVENT_SOURCE, qDef.getSourceFilters()));
                }
                s.setFilter(filterList);
                return s;
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot execute command", e);
        }
    }
        
    private Filter createFilterListOR(byte[] columnName, Set<String> values) {
        FilterList filterOR = new FilterList(FilterList.Operator.MUST_PASS_ONE);
        for (String actionValue : values) {
            filterOR.addFilter(
                    new SingleColumnValueFilter(B_AUDIT_CF, columnName, 
                            CompareOp.EQUAL, Bytes.toBytes(actionValue)));
        }
        return filterOR;
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
        Util.assertHasLength(new String[]{uuid});
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                Get queryGetById = new Get(Bytes.toBytes(uuid));
                return MAPPER.fromStore(table.get(queryGetById));
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot check feature existence", e);
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_NAME);
    }
    
    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_HOSTNAME);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_USER);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(query, COL_EVENT_SOURCE);
    }
    
    /** {@inheritDoc} */
    private Map<String, MutableHitCount> computeHitCount(EventQueryDefinition query, String pColName) {
        Map < String, MutableHitCount > hitCount = new HashMap<String, MutableHitCount>();
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                query.getActionFilters().add(ACTION_CHECK_OK);
                Scan scanQuery = buildQuery(query, Util.set(pColName), EventConstants.TARGET_FEATURE);
                try(ResultScanner scanner = table.getScanner(scanQuery)) {
                    for (Result rr = scanner.next(); rr != null; rr = scanner.next()) {
                        String colValue = Bytes.toString(rr.getValue(B_AUDIT_CF, Bytes.toBytes(pColName)));
                        if (hitCount.containsKey(colValue)) {
                            hitCount.get(colValue).inc();
                        } else {
                            hitCount.put(colValue, new MutableHitCount(1));
                        }
                     }
                }
            }
        } catch (IOException e) {
            throw new AuditAccessException("Compute hitcount based on " + pColName, e);
        }
        return hitCount;
    }
    
    /** {@inheritDoc} */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
        // Create the interval depending on units
        TimeSeriesChart tsc = new TimeSeriesChart(query.getFrom(), query.getTo(), units);
        // Search All events
        Iterator<Event> iterEvent = searchFeatureUsageEvents(query).iterator();
        // Dispatch events into time slots
        while (iterEvent.hasNext()) {
            tsc.addEvent(iterEvent.next());
        }
        return tsc;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        EventSeries es = new EventSeries();
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                query.getActionFilters().add(ACTION_CHECK_OK);
                
                Scan scanQuery = buildQuery(query, COLS_EVENT, EventConstants.TARGET_FEATURE);
                try(ResultScanner scanner = table.getScanner(scanQuery)) {
                    for (Result rr = scanner.next(); rr != null; rr = scanner.next()) {
                        es.add(MAPPER.fromStore(rr));
                     }
                }
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot search feature usage ", e);
        }
        return es;
    }

    /** {@inheritDoc} */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        EventSeries es = new EventSeries();
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                query.getActionFilters().add(ACTION_CHECK_OK);
                Scan scanQuery = buildQuery(query, COLS_EVENT, null);
                Set < String > candidates = Util.set(ACTION_DISCONNECT, 
                        ACTION_TOGGLE_ON, ACTION_TOGGLE_OFF,
                        ACTION_CREATE, ACTION_DELETE,
                        ACTION_UPDATE, ACTION_CLEAR);
                try(ResultScanner scanner = table.getScanner(scanQuery)) {
                    for (Result rr = scanner.next(); rr != null; rr = scanner.next()) {
                        String action = Bytes.toString(rr.getValue(B_AUDIT_CF, B_EVENT_ACTION));
                        if (candidates.contains(action)) {
                            es.add(MAPPER.fromStore(rr));
                        }
                     }
                }
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot search audit trail ", e);
        }
        return es;
    }
    
    /** {@inheritDoc} */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        this.purgeAuditTrail(query);
    }    

    /** {@inheritDoc} */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        try (Connection hbConn = ConnectionFactory.createConnection(conn.getConfig())) {
            try(Table table = hbConn.getTable(AUDIT_TABLENAME)) {
                query.getActionFilters().add(ACTION_CHECK_OK);
                // Scan for ids
                Scan scanQuery = buildQuery(query, Util.set(COL_EVENT_UID), null);
                List < Delete > list = new ArrayList<Delete>();
                try(ResultScanner scanner = table.getScanner(scanQuery)) {
                    for (Result rr = scanner.next(); rr != null; rr = scanner.next()) {
                        list.add(new Delete(rr.getValue(B_AUDIT_CF, B_EVENT_UID)));
                    }
                }
                table.delete(list);
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot search audit trail ", e);
        }
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
