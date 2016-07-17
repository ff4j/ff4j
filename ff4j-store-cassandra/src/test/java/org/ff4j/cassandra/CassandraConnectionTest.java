package org.ff4j.cassandra;

import org.ff4j.FF4j;
import org.ff4j.cassandra.store.FeatureStoreCassandra;

public class CassandraConnectionTest {

    public void testConnection() {
        
        CassandraConnection conn = new CassandraConnection();
        conn.createSchema();
        
        FF4j ff4j = new FF4j();
        ff4j.setFeatureStore(new FeatureStoreCassandra(conn));

        ff4j.getFeatureStore().exist("f1");
        
        conn.close();
    }
    
}
