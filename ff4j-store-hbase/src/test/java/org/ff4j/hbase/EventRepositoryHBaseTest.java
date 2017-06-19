package org.ff4j.hbase;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.hbase.store.EventRepositoryHBase;
import org.ff4j.test.audit.EventRepositoryTestSupport;

/**
 * Unit testing of implementation of {@link EventRepository} for HBASE Technology.
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryHBaseTest extends EventRepositoryTestSupport {

    /** HBASE_HOST. */
    private static final String HBASE_HOST = "hbase";
    
    /** HBASE_PORT. */
    private static final int HBASE_PORT = 2181;
    
    /** {@inheritDoc} */
    @Override
    protected EventRepository initRepository() {
        EventRepository hBaseEventRepo = new EventRepositoryHBase(new HBaseConnection(HBASE_HOST, HBASE_PORT, false));
        hBaseEventRepo.createSchema();
        return hBaseEventRepo;
    }

}
