package org.ff4j.cassandra.docker;

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

import org.ff4j.cassandra.AsbtractEventRepositoryCassandraTest;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.testcontainers.containers.CassandraContainer;
import org.testcontainers.containers.GenericContainer;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.CqlSessionBuilder;
import com.datastax.oss.driver.api.querybuilder.SchemaBuilder;

public class EventRepositoryassandraTestDocker extends AsbtractEventRepositoryCassandraTest {
    
    /** Cassandra Containers. **/
    protected static GenericContainer<?> cassandraContainer = null;
    
    /** Constants. **/
    private static final String KEYSPACE_NAME           = "ff4j";
    private static final String DOCKER_CASSANDRA_DC     = "datacenter1";
    private static final String DOCKER_CASSANDRA_IMAGE  = "cassandra:3.11.7";
    
    @BeforeClass
    public static void startDocker() throws Exception {
        cassandraContainer = new CassandraContainer<>(DOCKER_CASSANDRA_IMAGE);
        cassandraContainer.start();
    }
    
    @AfterClass
    public static void stopDockker() {
        cassandraContainer.stop();
    }

    /** {@inheritDoc} */
    @Override
    public CqlSession initCqlSession() {
        // Create Keyspace Before Tests
        try (CqlSession cqlSession = CqlSession.builder()
                .addContactPoint(new InetSocketAddress(
                        cassandraContainer.getContainerIpAddress(), 
                        cassandraContainer.getMappedPort(9042)))
                .withLocalDatacenter(DOCKER_CASSANDRA_DC)
                .build()) {
            cqlSession.execute(SchemaBuilder.createKeyspace(KEYSPACE_NAME)
                    .ifNotExists().withSimpleStrategy(1)
                    .withDurableWrites(true).build());
        }
        return new CqlSessionBuilder()
                .addContactPoint(new InetSocketAddress(
                        cassandraContainer.getContainerIpAddress(), 
                        cassandraContainer.getMappedPort(9042)))
                .withLocalDatacenter(DOCKER_CASSANDRA_DC)
                .withKeyspace(KEYSPACE_NAME)
                .build();
    }

}
