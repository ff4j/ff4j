package org.ff4j.test.store;

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.InMemoryEventRepository;
import org.ff4j.test.audit.EventRepositoryTestSupport;

/**
 * Sample test to illustrate usage of event repository
 * @author Cedrick LUNVEN (@clunven)
 */
public class InMemoryEventStoreTest extends EventRepositoryTestSupport {
    
    /** {@inheritDoc} */
    @Override
    protected EventRepository initRepository() {
        return new InMemoryEventRepository(60);
    }

}
