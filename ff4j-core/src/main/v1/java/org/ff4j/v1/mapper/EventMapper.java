package org.ff4j.v1.mapper;

import org.ff4j.v1.audit.Event;

/**
 * Specialization of mapper for Events
 * 
 * @author Cedrick LUNVEN (@clunven)
 *
 * @param <STORE_OBJ>
 */
public interface EventMapper < STORE_OBJ > extends Mapper<Event, STORE_OBJ> {}
