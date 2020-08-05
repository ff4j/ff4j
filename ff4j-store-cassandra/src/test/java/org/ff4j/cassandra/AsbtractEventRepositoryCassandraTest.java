package org.ff4j.cassandra;

import org.ff4j.cassandra.store.EventRepositoryCassandra;
import org.ff4j.test.audit.EventRepositoryTestSupport;
import org.junit.AfterClass;

import com.datastax.oss.driver.api.core.CqlSession;

/**
 * Test Cassandra Store.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public abstract class AsbtractEventRepositoryCassandraTest extends EventRepositoryTestSupport {
   
    protected static CqlSession cqlSession;
    
    public abstract CqlSession initCqlSession();
    
    /** {@inheritDoc} */
    @Override
    protected EventRepositoryCassandra initRepository() {
        if (null == cqlSession) cqlSession = initCqlSession();
        EventRepositoryCassandra store = new EventRepositoryCassandra(cqlSession);
        store.createSchema();
        return store;
    }
    
    @AfterClass
    public static void closeConnection() {
        cqlSession.close();
        cqlSession = null;
    }
}
