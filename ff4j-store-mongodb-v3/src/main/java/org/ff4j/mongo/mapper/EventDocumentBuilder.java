package org.ff4j.mongo.mapper;

import com.mongodb.client.model.Accumulators;
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.Filters;
import org.bson.Document;
import org.bson.conversions.Bson;
import org.ff4j.audit.EventQueryDefinition;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import static org.ff4j.audit.EventConstants.*;
import static org.ff4j.mongo.MongoDbConstants.EVENT_UUID;

/*
 * #%L
 * ff4j-store-mongodb
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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
 * Mongo object builder.
 *
 * @author Curtis White (@drizztguen77)
 */
public final class EventDocumentBuilder {

    /**
     * Mongo v3 document builder.
     */
    private final Document builder = new Document();

    /**
     * Mongo internal object representing attribute id.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getEventUuid(String value) {
        return new Document(EVENT_UUID, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addEventUuid(String value) {
        builder.append(EVENT_UUID, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'name'.
     *
     * @param value target value
     * @return internal mong object
     */
    public Document getName(String value) {
        return new Document(ATTRIBUTE_NAME, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addName(String value) {
        builder.append(ATTRIBUTE_NAME, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'host'.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getHost(String value) {
        return new Document(ATTRIBUTE_HOST, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addHost(String value) {
        builder.append(ATTRIBUTE_HOST, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'user'.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getUser(String value) {
        return new Document(ATTRIBUTE_USER, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addUser(String value) {
        builder.append(ATTRIBUTE_USER, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'source'.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getSource(String value) {
        return new Document(ATTRIBUTE_SOURCE, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addSource(String value) {
        builder.append(ATTRIBUTE_SOURCE, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'type'.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getType(String value) {
        return new Document(ATTRIBUTE_TYPE, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addType(String value) {
        builder.append(ATTRIBUTE_TYPE, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'action'.
     *
     * @param value target value
     * @return internal mongo object
     */
    public Document getAction(String value) {
        return new Document(ATTRIBUTE_ACTION, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return
     */
    public EventDocumentBuilder addAction(String value) {
        builder.append(ATTRIBUTE_ACTION, value);
        return this;
    }

    public List<Bson> getSelectFeatureUsageQuery(EventQueryDefinition eqd) {
        return buildAggregateFilters(eqd, true,false);
    }

    public List<Bson> buildHitCountFilters(EventQueryDefinition qDef, String attr) {
        return Arrays.asList(
                Aggregates.match(Filters.eq(ATTRIBUTE_TYPE, TARGET_FEATURE)),
                Aggregates.match(Filters.eq(ATTRIBUTE_ACTION, ACTION_CHECK_OK)),
                Aggregates.match(Filters.gte(ATTRIBUTE_TIME, qDef.getFrom())),
                Aggregates.match(Filters.lte(ATTRIBUTE_TIME, qDef.getTo())),
                Aggregates.group("$" + attr, Accumulators.sum("NB", 1)));
    }

    public List<Bson> buildAggregateFilters(EventQueryDefinition qDef, boolean filterForCheck, boolean filterAuditTrail) {
        List<Bson> filters = new ArrayList<>();

        filters.add(Aggregates.match(Filters.gte(ATTRIBUTE_TIME, qDef.getFrom())));
        filters.add(Aggregates.match(Filters.lte(ATTRIBUTE_TIME, qDef.getTo())));

        // If a dedicated filter is there use it
        if (qDef.getActionFilters().isEmpty()) {
            if (filterForCheck) {
                qDef.getActionFilters().add(ACTION_CHECK_OK);
            }
            if (filterAuditTrail) {
                qDef.getActionFilters().add(ACTION_CONNECT);
                qDef.getActionFilters().add(ACTION_DISCONNECT);
                qDef.getActionFilters().add(ACTION_TOGGLE_ON);
                qDef.getActionFilters().add(ACTION_TOGGLE_OFF);
                qDef.getActionFilters().add(ACTION_CREATE);
                qDef.getActionFilters().add(ACTION_DELETE);
                qDef.getActionFilters().add(ACTION_UPDATE);
                qDef.getActionFilters().add(ACTION_CLEAR);
            }
        }
        if (qDef.getActionFilters() != null && !qDef.getActionFilters().isEmpty()) {
            filters.add(Aggregates.match(Filters.in(ATTRIBUTE_ACTION, qDef.getActionFilters())));
        }
        if (qDef.getHostFilters() != null && !qDef.getHostFilters().isEmpty()) {
            filters.add(Aggregates.match(Filters.in(ATTRIBUTE_HOST, qDef.getHostFilters())));
        }
        if (qDef.getNamesFilter() != null && !qDef.getNamesFilter().isEmpty()) {
            filters.add(Aggregates.match(Filters.in(ATTRIBUTE_NAME, qDef.getNamesFilter())));
        }
        if (qDef.getSourceFilters() != null && !qDef.getSourceFilters().isEmpty()) {
            filters.add(Aggregates.match(Filters.in(ATTRIBUTE_SOURCE, qDef.getSourceFilters())));
        }

        return filters;
    }


    /**
     * Builder pattern.
     *
     * @return
     */
    public Document build() {
        return builder;
    }
}
