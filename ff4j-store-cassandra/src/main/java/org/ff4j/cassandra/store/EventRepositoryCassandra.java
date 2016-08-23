package org.ff4j.cassandra.store;

/*
 * #%L
 * ff4j-store-cassandra
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


import org.ff4j.audit.Event;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.cassandra.CassandraConnection;

/**
 * Implementation of audit into Cassandra DB
 *
 * @Abstract as note implemented yet.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class EventRepositoryCassandra extends AbstractEventRepository {
    
    /** TLL to working with ' expiring columns' if positive number. */
    private int ttl = -1;
    
    /** Connection to store Cassandra. */
    private CassandraConnection conn;
    
    /** {@inheritDoc} */
    @Override
    public boolean saveEvent(Event e) {
        String query =  "INSERT INTO...";
        if (ttl > 0) {
            query += "USING TTL " + ttl;
        }
        return true;
    }

    /**
     * Getter accessor for attribute 'ttl'.
     *
     * @return
     *       current value of 'ttl'
     */
    public int getTtl() {
        return ttl;
    }

    /**
     * Setter accessor for attribute 'ttl'.
     * @param ttl
     * 		new value for 'ttl '
     */
    public void setTtl(int ttl) {
        this.ttl = ttl;
    }

    /**
     * Getter accessor for attribute 'conn'.
     *
     * @return
     *       current value of 'conn'
     */
    public CassandraConnection getConn() {
        return conn;
    }

    /**
     * Setter accessor for attribute 'conn'.
     * @param conn
     * 		new value for 'conn '
     */
    public void setConn(CassandraConnection conn) {
        this.conn = conn;
    }
}
