package org.ff4j;

import org.ff4j.event.Event;

/**
 * Public Interface of a Listener on CRUD repository.
 * 
 * @Do not put any onRead() as making not sense in ff4J.
 * 
 * @author Cedrick LUNVEN  (@clunven)
 *
 * @param <ENTITY>
 *    {@link FF4jEntity} to be specialized by type of store 
 */
public interface FF4jRepositoryEventListener extends FF4jRepositoryListener< Event > {
    
    
}
