package org.ff4j.elastic.store;

/*
 * #%L
 * ff4j-store-elastic
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;

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
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.utils.Util;

import io.searchbox.client.JestResult;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;

public class EventRepositoryElastic extends AbstractEventRepository {

	private ElasticConnection connection;

	/** Connection to ElasticSearch query builder */
	private ElasticQueryBuilder builder;

	public EventRepositoryElastic(ElasticConnection connection) {
		this.connection = connection;
	}

	@Override
	public boolean saveEvent(Event event) {
		Util.assertEvent(event);
		JestResult result = getConnection().execute(getBuilder().queryCreateEvent(event));
		return (result != null && result.isSucceeded());
	}

	@Override
	public Event getEventByUUID(String uuid, Long timestamp) {
		return getConnection().execute(getBuilder().queryGetEventById(uuid)).getSourceAsObject(Event.class);
	}

	@Override
	public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
		JestResult result = getConnection()
				.execute(getBuilder().queryGetEventQueryDefinition(query, EventConstants.ACTION_CHECK_OK));
		List<Event> events = result.getSourceAsObjectList(Event.class);
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
	}

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

	@Override
	public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
		JestResult result = getConnection()
				.execute(getBuilder().queryGetEventQueryDefinition(query, EventConstants.ACTION_CHECK_OK));
		List<Event> events = result.getSourceAsObjectList(Event.class);
		EventSeries es = new EventSeries();
		for (Event event : events) {
			es.add(event);
		}
		return es;
	}

	@Override
	public void purgeFeatureUsage(EventQueryDefinition query) {
		this.purgeAuditTrail(query);
	}

	@Override
	public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
		JestResult result = getConnection()
				.execute(getBuilder().queryGetEventQueryDefinition(query, EventConstants.ACTION_CHECK_OK));
		List<Event> events = result.getSourceAsObjectList(Event.class);
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
	}

	@Override
	public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
		JestResult result = getConnection()
				.execute(getBuilder().queryGetEventQueryDefinition(query, EventConstants.ACTION_CHECK_OK));
		List<Event> events = result.getSourceAsObjectList(Event.class);
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
	}

	@Override
	public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
		JestResult result = getConnection()
				.execute(getBuilder().queryGetEventQueryDefinition(query, EventConstants.ACTION_CHECK_OK));
		List<Event> events = result.getSourceAsObjectList(Event.class);
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
	}

	@Override
	public EventSeries getAuditTrail(EventQueryDefinition query) {
		JestResult result = getConnection().execute(getBuilder().queryGetEventQueryDefinition(query));
		List<Event> events = result.getSourceAsObjectList(Event.class);
		EventSeries es = new EventSeries();
		Set<String> candidates = Util.set(ACTION_DISCONNECT, //
				ACTION_TOGGLE_ON, ACTION_TOGGLE_OFF, ACTION_CREATE, //
				ACTION_DELETE, ACTION_UPDATE, ACTION_CLEAR);
		for (Event event : events) {
			if (candidates.contains(event.getAction()))
				es.add(event);
		}
		return es;
	}

	@Override
	public void purgeAuditTrail(EventQueryDefinition query) {
		SearchResult result = getConnection().search(getBuilder().queryReadAllEvents(), true);
		if (result != null && result.isSucceeded()) {
			for (Hit<Event, Void> event : result.getHits(Event.class)) {
				getConnection().execute(getBuilder().queryDeleteEvent(event.source.getUuid()));
			}
		}
	}

	@Override
	public void createSchema() {
		getConnection().execute(getBuilder().queryFlushIndex());
	}

	/**
	 * Getter accessor for attribute 'connection'.
	 *
	 * @return current value of 'connection'
	 */
	public ElasticConnection getConnection() {
		return connection;
	}

	/**
	 * Setter accessor for attribute 'connection'.
	 * 
	 * @param connection
	 *            new value for 'connection '
	 */
	public void setConnection(ElasticConnection connection) {
		this.connection = connection;
	}

	/**
	 * Getter accessor for attribute 'builder'.
	 *
	 * @return current value of 'builder'
	 */
	public ElasticQueryBuilder getBuilder() {
		if (builder == null) {
			builder = new ElasticQueryBuilder(connection);
		}
		return builder;
	}
}
