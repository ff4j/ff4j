package org.ff4j.elastic.store;

/*-
 * #%L
 * ff4j-store-elastic
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

import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_DISCONNECT;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CHECK_OFF;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.elastic.ElasticQueryBuilder.createEvent;
import static org.ff4j.elastic.ElasticQueryBuilder.deleteEvent;
import static org.ff4j.elastic.ElasticQueryBuilder.findEventById;
import static org.ff4j.elastic.ElasticQueryBuilder.findEventsFromQueryDefinition;
import static org.ff4j.elastic.ElasticQueryHelper.createIndexIfNotExist;
import static org.ff4j.elastic.ElasticQueryHelper.findEventTechIdFromUid;

import java.io.IOException;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.TimeUnit;

import org.ff4j.audit.Event;
import org.ff4j.audit.EventConstants;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.client.JestClient;
import io.searchbox.core.Index;
import io.searchbox.core.Search;

/**
 * Implementation of {@link EventRepository} in Elastic 6+.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class EventRepositoryElastic extends AbstractEventRepository {

    /** Logger for the class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(EventRepositoryElastic.class);
   
    /** if no value provide for index use this one. */
    public static String DEFAULT_INDEX_EVENT = "ff4j_events";
    
    /** Injection of connection to elastic. */
    private JestClient jestClient;
    
    /** Default name of the index in elastic. */
    private String indexEvents = DEFAULT_INDEX_EVENT;

    /**
     * Default constructor.
     */
    public EventRepositoryElastic() {}

    /**
     * Initialization through {@link ElasticConnection}.
     *
     * @param connection
     *            current client to Elasticsearch database
     */
    public EventRepositoryElastic(JestClient jestClient) {
        this(jestClient, DEFAULT_INDEX_EVENT);
    }

    /**
     * Initialization with Connection and initialisation file.
     *
     * @param connection
     * @param xmlFile
     */
    public EventRepositoryElastic(JestClient jestClient, String indexName) {
        this.jestClient    = jestClient;
        this.indexEvents = indexName;
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        LOGGER.info("Creating index {} (if needed)", indexEvents);
        createIndexIfNotExist(jestClient, indexEvents);
    }

	/** {@inheritDoc} */
	@Override
	public boolean saveEvent(Event event) {
		Util.assertEvent(event);
		try {
            Index creationQuery = createEvent(indexEvents, event);
            return jestClient.execute(creationQuery).isSucceeded();
        } catch (IOException e) {
            throw new AuditAccessException("Cannot create event '" + event.getUuid() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("deprecation")
    public Event getEventByUUID(String uuid, Long timestamp) {
	    try {
            Search search = findEventById(indexEvents, uuid);
            return jestClient.execute(search).getSourceAsObject(Event.class);
        } catch (IOException e) {
            throw new AuditAccessException("Cannot read event '" + uuid + "'", e);
        }
	}

	/** {@inheritDoc} */
	@SuppressWarnings("deprecation")
    @Override
	public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
	    try {
	        Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  EventConstants.ACTION_CHECK_OK);
	        List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
	        Map<String, MutableHitCount> hitCount = new HashMap<String, MutableHitCount>();
	        for (Event event : events) {
	            String name = event.getName();
	            if (hitCount.containsKey(name)) {
	                hitCount.get(name).inc();
	            } else {
	                hitCount.put(name, new MutableHitCount(1));
	            }
	        }
	        return hitCount;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	    
	}

	/** {@inheritDoc} */
	@Override
	public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit units) {
		// Create the interval depending on units
		TimeSeriesChart tsc = new TimeSeriesChart(query.getFrom(), query.getTo(), units);
		// Search All events
		Iterator<Event> iterEvent = searchFeatureUsageEvents(query).iterator();
		// Dispatch events into time slots
		while (iterEvent.hasNext()) {
			tsc.addEvent(iterEvent.next());
		}
		return tsc;
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("deprecation")
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  EventConstants.ACTION_CHECK_OK);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            EventSeries es = new EventSeries();
    		for (Event event : events) {
    			es.add(event);
    		}
    		return es;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@SuppressWarnings("deprecation")
    @Override
	public void purgeFeatureUsage(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  null);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            if (null != events) {
                // Audit Actions
                Set<String> candidates = Util.set(ACTION_CHECK_OK, ACTION_CHECK_OFF);
                for (Event event : events) {
                   if (candidates.contains(event.getAction())) {
                        String uuid = event.getUuid();
                        String techIs = findEventTechIdFromUid(jestClient, indexEvents, uuid);
                        jestClient.execute(deleteEvent(indexEvents, techIs, uuid));
                    }
                }
            }
        } catch (IOException e) {
            throw new AuditAccessException("Cannot purge events '" + query.toString() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@SuppressWarnings("deprecation")
    @Override
	public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  EventConstants.ACTION_CHECK_OK);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            Map<String, MutableHitCount> hitCount = new HashMap<String, MutableHitCount>();
    		for (Event event : events) {
    			String hostName = event.getHostName();
    			if (hitCount.containsKey(hostName)) {
    				hitCount.get(hostName).inc();
    			} else {
    				hitCount.put(hostName, new MutableHitCount(1));
    			}
    		}
    		return hitCount;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	}

    /** {@inheritDoc} */
    @Override
    @SuppressWarnings("deprecation")
	public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  EventConstants.ACTION_CHECK_OK);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
       		Map<String, MutableHitCount> hitCount = new HashMap<String, MutableHitCount>();
    		for (Event event : events) {
    			String user = event.getUser();
    			if (hitCount.containsKey(user)) {
    				hitCount.get(user).inc();
    			} else {
    				hitCount.put(user, new MutableHitCount(1));
    			}
    		}
    		return hitCount;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("deprecation")
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  EventConstants.ACTION_CHECK_OK);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            Map<String, MutableHitCount> hitCount = new HashMap<String, MutableHitCount>();
    		for (Event event : events) {
    			String source = event.getSource();
    			if (hitCount.containsKey(source)) {
    				hitCount.get(source).inc();
    			} else {
    				hitCount.put(source, new MutableHitCount(1));
    			}
    		}
    		return hitCount;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("deprecation")
    public EventSeries getAuditTrail(EventQueryDefinition query) {
	    try {
            Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  null);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            EventSeries es = new EventSeries();
    		Set<String> candidates = Util.set(ACTION_DISCONNECT, //
    				ACTION_TOGGLE_ON, ACTION_TOGGLE_OFF, ACTION_CREATE, //
    				ACTION_DELETE, ACTION_UPDATE, ACTION_CLEAR);
    		for (Event event : events) {
    			if (candidates.contains(event.getAction()))
    				es.add(event);
    		}
    		return es;
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot read events '" + query.toString() + "'", e);
        }
	}

	/** {@inheritDoc} */
	@Override
	@SuppressWarnings("deprecation")
    public void purgeAuditTrail(EventQueryDefinition query) {
	    try {
	        Search queryEvents = findEventsFromQueryDefinition(indexEvents, query,  null);
            List<Event> events = jestClient.execute(queryEvents).getSourceAsObjectList(Event.class);
            if (null != events) {
                // Audit Actions
                Set<String> candidates = Util.set(ACTION_DISCONNECT, //
                        ACTION_TOGGLE_ON, ACTION_TOGGLE_OFF, ACTION_CREATE, //
                        ACTION_DELETE, ACTION_UPDATE, ACTION_CLEAR);
                for (Event event : events) {
                   if (candidates.contains(event.getAction())) {
                        String uuid = event.getUuid();
                        String techIs = findEventTechIdFromUid(jestClient, indexEvents, uuid);
                        jestClient.execute(deleteEvent(indexEvents, techIs, uuid));
                    }
                }
            }
	    } catch (IOException e) {
            throw new AuditAccessException("Cannot purge events '" + query.toString() + "'", e);
        }
	}

    /**
     * Getter accessor for attribute 'indexEvents'.
     *
     * @return
     *       current value of 'indexEvents'
     */
    public String getIndexEvents() {
        return indexEvents;
    }

    /**
     * Setter accessor for attribute 'indexEvents'.
     * @param indexEvents
     * 		new value for 'indexEvents '
     */
    public void setIndexEvents(String indexEvents) {
        this.indexEvents = indexEvents;
    }	
}
