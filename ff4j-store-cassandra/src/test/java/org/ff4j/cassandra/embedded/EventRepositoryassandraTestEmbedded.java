package org.ff4j.cassandra.embedded;

import java.net.InetSocketAddress;

import org.cassandraunit.utils.EmbeddedCassandraServerHelper;
import org.ff4j.cassandra.AsbtractEventRepositoryCassandraTest;
import org.junit.BeforeClass;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.CqlSessionBuilder;
import com.datastax.oss.driver.api.querybuilder.SchemaBuilder;

public class EventRepositoryassandraTestEmbedded extends AsbtractEventRepositoryCassandraTest {
    
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
