package org.ff4j.cassandra.docker;

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
