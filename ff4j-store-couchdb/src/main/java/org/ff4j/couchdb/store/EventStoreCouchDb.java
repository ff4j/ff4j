package org.ff4j.couchdb.store;

/*-
 * #%L
 * ff4j-store-couchdb
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

import org.ektorp.CouchDbConnector;
import org.ektorp.DocumentNotFoundException;
import org.ektorp.UpdateConflictException;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.couchdb.CouchDbConnection;
import org.ff4j.couchdb.CouchDbEventView;
import org.ff4j.couchdb.document.CouchDbEvent;
import org.ff4j.utils.json.EventJsonParser;
import org.lightcouch.CouchDbException;

import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

import static org.ff4j.couchdb.CouchDbConstants.DEFAULT_EVENT_TYPE;

/**
 * Implementation of CouchDbEventView for Mongo.
 *
 * @author Curtis White (@drizztguen77)
 */
public class EventStoreCouchDb extends AbstractEventRepository {

    /**
     * Current CouchDB connection.
     */
    private CouchDbConnection couchDbConnection;

    /**
     * Current CouchDB connector.
     */
    private CouchDbConnector couchDbConnector;

    /**
     * Getter accessor for attribute 'couchDbConnection'.
     *
     * @return
     *       current value of 'couchDbConnection'
     */
    public CouchDbConnection getCouchDbConnection() {
        return couchDbConnection;
    }

    /**
     * Getter accessor for attribute 'couchDbConnector'.
     *
     * @return
     *       current value of 'couchDbConnector'
     */
    public CouchDbConnector getCouchDbConnector() {
        return couchDbConnector;
    }

    /**
     * Getter accessor for attribute 'couchDbEventView'.
     *
     * @return
     *       current value of 'couchDbEventView'
     */
    public CouchDbEventView getCouchDbEventView() {
        return couchDbEventView;
    }

    /**
     * Repository class to query couchDB
     */
    private CouchDbEventView couchDbEventView;

    /**
     * Default constructor
     */
    public EventStoreCouchDb() {
    }

    /**
     * Parameterized constructor with database connection.
     *
     * @param couchDbConnection the database connection to set
     * @param couchDbEventView  event repository
     */
    public EventStoreCouchDb(CouchDbConnection couchDbConnection, CouchDbEventView couchDbEventView) {
        this.couchDbConnection = couchDbConnection;
        this.couchDbConnector = couchDbConnection.getCouchDbConnector();
        this.couchDbEventView = couchDbEventView;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        // Do nothing here since CouchDB does not have collection or any other table
        // structure. There is nothing to create
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveEvent(Event e) {
        if (e == null) {
            throw new IllegalArgumentException("Event cannot be null nor empty");
        }

        try {
            // Try to get the event. If one is returned then update it
            CouchDbEvent couchDbEvent = getEvent(e.getUuid());
            couchDbEvent.setEvent(e.toJson());
            updateEvent(couchDbEvent);

        } catch (DocumentNotFoundException dnf) {
            // If no event was found then create a new one
            CouchDbEvent couchDbEvent = new CouchDbEvent();
            couchDbEvent.setType(DEFAULT_EVENT_TYPE);
            couchDbEvent.setEvent(e.toJson());
            couchDbEvent.setId(e.getUuid());
            createEvent(couchDbEvent);
        }

        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        CouchDbEvent couchDbEvent = getEvent(uuid);
        if (couchDbEvent == null) return null;

        return fromJson(couchDbEvent.getEvent());
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        // Not implemented
        return new TimeSeriesChart();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
        // Not implemented
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        // Not implemented
        return new HashMap<>();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        // Not implemented
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
        // Not implemented
    }

    /**
     * Gets a list of all events in the database
     *
     * @return list of CouchDB events
     * @throws CouchDbException If the query failed to execute or the request is invalid.
     */
    public List<CouchDbEvent> getEvents() {
        return this.couchDbEventView.getAll();
    }

    /**
     * Get a single event from CouchDB
     *
     * @param uid UUID of the event to get
     * @return the CouchDB event
     * @throws DocumentNotFoundException If the document is not found in the database.
     */
    public CouchDbEvent getEvent(String uid) {
        return this.couchDbEventView.get(uid);
    }

    /**
     * Create a single CouchDB event.
     *
     * @param couchDbEvent CouchDB event to create
     * @throws UpdateConflictException If a conflict is detected during the create.
     */
    public void createEvent(CouchDbEvent couchDbEvent) {
        this.couchDbEventView.add(couchDbEvent);
    }

    /**
     * Update a single CouchDB event
     *
     * @param couchDbEvent CouchDB event to update
     * @throws UpdateConflictException If a conflict is detected during the update.
     */
    public void updateEvent(CouchDbEvent couchDbEvent) {
        this.couchDbEventView.update(couchDbEvent);
    }

    /**
     * Update a single CouchDB event
     *
     * @param couchDbEvent CouchDB event to update
     * @throws UpdateConflictException If the document is not found in the database.
     */
    public void removeEvent(CouchDbEvent couchDbEvent) {
        this.couchDbEventView.remove(couchDbEvent);
    }

    private Event fromJson(String json) {
        return EventJsonParser.parseEvent(json);
    }


    /**
     * Setter to set the CouchDB connection
     *
     * @param couchDbConnection CouchDB connection
     */
    public void setCouchDbConnector(CouchDbConnection couchDbConnection) {
        this.couchDbConnection = couchDbConnection;
    }

    /**
     * Setter to set the event repository
     *
     * @param couchDbEventView Event repository
     */
    public void setCouchDbEventView(CouchDbEventView couchDbEventView) {
        this.couchDbEventView = couchDbEventView;
    }

}
