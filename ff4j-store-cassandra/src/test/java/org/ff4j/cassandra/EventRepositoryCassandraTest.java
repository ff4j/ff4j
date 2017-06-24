package org.ff4j.cassandra;

import org.cassandraunit.utils.EmbeddedCassandraServerHelper;

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

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.cassandra.store.EventRepositoryCassandra;
import org.ff4j.test.audit.EventRepositoryTestSupport;
import org.junit.BeforeClass;
import org.junit.Ignore;
import org.junit.Test;

/**
 * Test For {@link EventRepository} to work with Cassandra {@link EventRepositoryCassandra}.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
//Cassandra embedded KO
@Ignore
public class EventRepositoryCassandraTest extends EventRepositoryTestSupport {

    /** Reuse the embedded server. */
    protected static CassandraConnection conn;
    
    @BeforeClass
    public static void startEmbeddedCassandra() throws Exception {
        // Use Cassandra-Unit 
        EmbeddedCassandraServerHelper.startEmbeddedCassandra(15000);
        conn = new CassandraConnection("127.0.0.1", 9142);
        // <--
        conn.createKeySpace();
    }
    
    /** {@inheritDoc} */
    @Override
    protected EventRepository initRepository() {
        EventRepository cassandraStore = new EventRepositoryCassandra(conn);
        cassandraStore.createSchema();
        return cassandraStore;
    }
    
    @Test
    public void testCustom() {
        EventRepositoryCassandra repoCassandra = (EventRepositoryCassandra) ff4j.getEventRepository();
        repoCassandra.setTtl(repoCassandra.getTtl());
        repoCassandra.setConn(repoCassandra.getConn());
        repoCassandra.setBuilder(repoCassandra.getBuilder());
        new EventRepositoryCassandra();
    }

}
