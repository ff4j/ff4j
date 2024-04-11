package org.ff4j.cassandra.embedded;

/*-
 * #%L
 * ff4j-store-cassandra
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import java.net.InetSocketAddress;

import org.cassandraunit.utils.EmbeddedCassandraServerHelper;
import org.ff4j.cassandra.AsbtractPropertyStoreCassandraTest;
import org.junit.BeforeClass;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.CqlSessionBuilder;
import com.datastax.oss.driver.api.querybuilder.SchemaBuilder;

public class PropertyStoreCassandraTestEmbedded extends AsbtractPropertyStoreCassandraTest {
    
    private static final String LOCAL_NODE_IP    = "127.0.0.1";
    private static final String LOCAL_NODE_DC    = "datacenter1";
    private static final String LOCAL_NODE_KS    = "ff4j";
    private static final int    LOCAL_NODE_PORT  = 9142;
   
    @BeforeClass
    public static void startEmbeddedCassandra() throws Exception {
        EmbeddedCassandraServerHelper.startEmbeddedCassandra(50000);
    }

    /** {@inheritDoc} */
    @Override
    public CqlSession initCqlSession() {
        // Create Keyspace Before Tests
        try (CqlSession cqlSession = CqlSession.builder()
                .addContactPoint(new InetSocketAddress(LOCAL_NODE_IP, LOCAL_NODE_PORT))
                .withLocalDatacenter(LOCAL_NODE_DC)
                .build()) {
            cqlSession.execute(SchemaBuilder.createKeyspace(LOCAL_NODE_KS)
                    .ifNotExists().withSimpleStrategy(1)
                    .withDurableWrites(true).build());
        }
        return new CqlSessionBuilder()
                .addContactPoint(new InetSocketAddress(LOCAL_NODE_IP, LOCAL_NODE_PORT))
                .withLocalDatacenter(LOCAL_NODE_DC)
                .withKeyspace(LOCAL_NODE_KS)
                .build();
    }

}
