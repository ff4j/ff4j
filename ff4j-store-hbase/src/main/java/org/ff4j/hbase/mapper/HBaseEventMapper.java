package org.ff4j.hbase.mapper;

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

import static org.ff4j.hbase.HBaseConstants.B_AUDIT_CF;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_ACTION;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_DATE;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_DURATION;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_HOSTNAME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_KEYS;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_NAME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_SOURCE;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_TIME;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_TYPE;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_UID;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_USER;
import static org.ff4j.hbase.HBaseConstants.B_EVENT_VALUE;

import java.text.SimpleDateFormat;

import org.apache.commons.lang.NotImplementedException;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.audit.Event;
import org.ff4j.mapper.EventMapper;
import org.ff4j.utils.MappingUtil;

/**
 * Mapping events into HBASE.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class HBaseEventMapper implements EventMapper<Put> {

    /** Create key. */
    protected static final SimpleDateFormat KDF = new SimpleDateFormat("yyyyMMdd");
    
    /** {@inheritDoc} */
    @Override
    public Put toStore(Event evt) {
        Put put = new Put(Bytes.toBytes(evt.getUuid()));
        put.addColumn(B_AUDIT_CF, B_EVENT_UID,      Bytes.toBytes(evt.getUuid()));
        put.addColumn(B_AUDIT_CF, B_EVENT_SOURCE,   Bytes.toBytes(evt.getSource()));
        put.addColumn(B_AUDIT_CF, B_EVENT_NAME,     Bytes.toBytes(evt.getName()));
        put.addColumn(B_AUDIT_CF, B_EVENT_ACTION,   Bytes.toBytes(evt.getAction()));
        put.addColumn(B_AUDIT_CF, B_EVENT_TYPE,     Bytes.toBytes(evt.getType()));
        put.addColumn(B_AUDIT_CF, B_EVENT_DURATION, Bytes.toBytes(evt.getDuration()));
        put.addColumn(B_AUDIT_CF, B_EVENT_HOSTNAME, Bytes.toBytes(evt.getHostName()));
        put.addColumn(B_AUDIT_CF, B_EVENT_USER,     Bytes.toBytes(evt.getUser()));
        put.addColumn(B_AUDIT_CF, B_EVENT_VALUE,    Bytes.toBytes(evt.getValue()));
        put.addColumn(B_AUDIT_CF, B_EVENT_DATE,     Bytes.toBytes(KDF.format(evt.getDate())));
        put.addColumn(B_AUDIT_CF, B_EVENT_TIME,     Bytes.toBytes(evt.getTimestamp()));
        put.addColumn(B_AUDIT_CF, B_EVENT_KEYS,     Bytes.toBytes(MappingUtil.fromMap(evt.getCustomKeys())));
        return put;
    }
    
    /** {@inheritDoc} */
    public Event fromStore(Result result) {
        if (result == null) return null;
        Event evt = new Event();
        evt.setUuid(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_UID)));
        evt.setSource(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_SOURCE)));
        evt.setType(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_TYPE)));
        evt.setName(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_NAME)));
        evt.setAction(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_ACTION)));
        evt.setDuration(Bytes.toLong(result.getValue(B_AUDIT_CF, B_EVENT_DURATION)));
        evt.setHostName(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_HOSTNAME)));
        evt.setUser(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_USER)));
        evt.setValue(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_VALUE)));
        evt.setTimestamp(Bytes.toLong(result.getValue(B_AUDIT_CF, B_EVENT_TIME)));
        evt.setCustomKeys(MappingUtil.toMap(Bytes.toString(result.getValue(B_AUDIT_CF, B_EVENT_KEYS))));
        return evt;
    }

    /** {@inheritDoc} */
    @Override
    public Event fromStore(Put bean) {
        throw new NotImplementedException("Data retrieved from HBASE are GET (not PUT)");
    }
    
   

}
