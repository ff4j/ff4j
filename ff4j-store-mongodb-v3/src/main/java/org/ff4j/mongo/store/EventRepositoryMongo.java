package org.ff4j.mongo.store;

import com.mongodb.Block;
import com.mongodb.MongoClient;
import com.mongodb.client.MongoCollection;
import com.mongodb.client.MongoDatabase;
import com.mongodb.client.model.Accumulators;
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.Filters;
import org.bson.Document;
import org.ff4j.audit.Event;
import org.ff4j.audit.EventQueryDefinition;
import org.ff4j.audit.EventSeries;
import org.ff4j.audit.MutableHitCount;
import org.ff4j.audit.chart.TimeSeriesChart;
import org.ff4j.audit.repository.AbstractEventRepository;
import org.ff4j.exception.AuditAccessException;
import org.ff4j.mongo.MongoDbConstants;
import org.ff4j.mongo.mapper.EventDocumentBuilder;
import org.ff4j.mongo.mapper.MongoEventMapper;

import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.function.Consumer;

import static org.ff4j.audit.EventConstants.*;

/*
 * #%L
 * ff4j-store-mongodb-v3
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

/**
 * Implementation of EventRepository for Mongo.
 *
 * @author Cedrick LUNVEN (@clunven)
 * @author Curtis White (@drizztguen77)
 */
public class EventRepositoryMongo extends AbstractEventRepository {

    /**
     * Event Mapping.
     */
    private static final MongoEventMapper EMAPPER = new MongoEventMapper();

    /**
     * Build fields.
     */
    private static final EventDocumentBuilder BUILDER = new EventDocumentBuilder();

    /**
     * MongoDB collection.
     */
    private MongoCollection<Document> eventsCollection;

    /**
     * Feature collection Name.
     */
    private String collectionName = MongoDbConstants.DEFAULT_EVENT_COLLECTION;

    /**
     * error message.
     */
    public static final String EVENT_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Event identifier cannot be null nor empty";

    /**
     * Database name.
     */
    private String dbName = MongoDbConstants.DEFAULT_DBNAME;

    /**
     * Current mongo client.
     */
    private MongoClient mongoClient;

    /**
     * Parameterized constructor with collection.
     *
     * @param client the mongo client
     */
    public EventRepositoryMongo(MongoClient client) {
        this(client, MongoDbConstants.DEFAULT_DBNAME);
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param client the mongo client
     * @param dbName the database name
     */
    public EventRepositoryMongo(MongoClient client, String dbName) {
        this.dbName = dbName;
        this.mongoClient = client;
        this.eventsCollection = getEventCollection();
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param events a list of events
     */
    public EventRepositoryMongo(MongoCollection<Document> events) {
        this.eventsCollection = events;
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param db the mongo database
     */
    public EventRepositoryMongo(MongoDatabase db) {
        this(db, MongoDbConstants.DEFAULT_EVENT_COLLECTION);
    }

    /**
     * Parameterized constructor with collection.
     *
     * @param db             the mongo database
     * @param collectionName collection name
     */
    public EventRepositoryMongo(MongoDatabase db, String collectionName) {
        this.eventsCollection = db.getCollection(collectionName);
    }

    /**
     * Getter accessor for attribute 'featuresCollection'.
     *
     * @return current value of 'featuresCollection'
     */
    public MongoCollection<Document> getEventCollection() {
        if (eventsCollection == null) {
            if (mongoClient != null) {
                createSchema();
            } else {
                throw new IllegalStateException("Cannot initialize Features collection : no mongo client defined");
            }
        }
        return eventsCollection;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void createSchema() {
        if (!mongoClient.getDatabase(dbName)
                .listCollectionNames()
                .into(new HashSet<>())
                .contains(collectionName)) {
            mongoClient.getDatabase(dbName).createCollection(collectionName);
        }
        eventsCollection = mongoClient.getDatabase(dbName).getCollection(collectionName);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public boolean saveEvent(Event e) {
        if (e == null) {
            throw new IllegalArgumentException("Event cannot be null nor empty");
        }
        eventsCollection.insertOne(EMAPPER.toStore(e));
        return true;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Event getEventByUUID(String uuid, Long timestamp) {
        if (uuid == null || uuid.isEmpty()) {
            throw new IllegalArgumentException(EVENT_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        Document object = getEventCollection().find(BUILDER.getEventUuid(uuid)).first();
        if (object == null) {
            throw new AuditAccessException(uuid);
        }
        return EMAPPER.fromStore(object);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getFeatureUsageHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_NAME);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getHostHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_HOST);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getUserHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_USER);
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public Map<String, MutableHitCount> getSourceHitCount(EventQueryDefinition query) {
        return computeHitCount(query, ATTRIBUTE_SOURCE);
    }

    /**
     * {@inheritDoc}
     */
    private Map<String, MutableHitCount> computeHitCount(EventQueryDefinition query, String attr) {

        Map<String, MutableHitCount> mapofHitCount = new HashMap<>();
        getEventCollection().aggregate(Arrays.asList(
                Aggregates.match(Filters.eq(ATTRIBUTE_TYPE, TARGET_FEATURE)),
                Aggregates.match(Filters.eq(ATTRIBUTE_ACTION, ACTION_CHECK_OK)),
                Aggregates.match(Filters.gte(ATTRIBUTE_TIME, query.getFrom())),
                Aggregates.match(Filters.lte(ATTRIBUTE_ACTION, query.getTo())),
                Aggregates.group("$" + attr, Accumulators.sum("NB", 1))
        )).forEach((Consumer<Document>) document -> {
            if (null != document.get("_id")) {
                mapofHitCount.put(document.get("_id").toString(), new MutableHitCount((Integer) document.get("NB")));
            } else {
                mapofHitCount.put(attr, new MutableHitCount(0));
            }
        });

        /*
            Need to do the following query but in mongo from JdbcQueryBuilder and JdbcStoreConstants

            StringBuilder sb = new StringBuilder();
            sb.append("SELECT count(" + COL_EVENT_UUID + ") as NB, " + columName + " FROM ");
            sb.append(getSchemaPattern());
            sb.append(getTableNameAudit());
            sb.append(" WHERE (" + COL_EVENT_TYPE   + " LIKE '" + EventConstants.TARGET_FEATURE  + "') ");
            sb.append(" AND   (" + COL_EVENT_ACTION + " LIKE '" + EventConstants.ACTION_CHECK_OK + "') ");
            sb.append(" AND   (" + COL_EVENT_TIME + "> ?) ");
            sb.append(" AND   (" + COL_EVENT_TIME + "< ?)");
            sb.append(" GROUP BY " + columName);

            Timestamps come from the query definition
            new Timestamp(query.getFrom()),
            new Timestamp(query.getTo()));

            Iterate through the returned list of documents which should contain the name and count. Should be able to use
            aggregation for the query
            https://docs.mongodb.com/manual/reference/operator/aggregation/group/
            https://docs.mongodb.com/manual/reference/operator/aggregation/count/

            hitcount goes into the following for each document
            new MutableHitCount(rs.getInt("NB"))
         */

        return mapofHitCount;
    }


    /**
     * {@inheritDoc}
     */
    @Override
    public TimeSeriesChart getFeatureUsageHistory(EventQueryDefinition query, TimeUnit tu) {
        return new TimeSeriesChart();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries searchFeatureUsageEvents(EventQueryDefinition query) {
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public EventSeries getAuditTrail(EventQueryDefinition query) {
        return new EventSeries();
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeAuditTrail(EventQueryDefinition query) {
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void purgeFeatureUsage(EventQueryDefinition query) {
    }
}
