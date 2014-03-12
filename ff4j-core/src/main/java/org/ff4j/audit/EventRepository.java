package org.ff4j.audit;

import java.util.List;

public interface EventRepository {

    boolean saveEvent(Event e);

    List<Event> getAllEvents();
    // getEventsPerFeatures(StartDate);

}
