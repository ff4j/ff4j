package org.ff4j.mongo.mapper;

/*-
 * #%L
 * ff4j-store-mongodb
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

import static org.ff4j.audit.EventConstants.ACTION_CHECK_OK;
import static org.ff4j.audit.EventConstants.ACTION_CLEAR;
import static org.ff4j.audit.EventConstants.ACTION_CONNECT;
import static org.ff4j.audit.EventConstants.ACTION_CREATE;
import static org.ff4j.audit.EventConstants.ACTION_DELETE;
import static org.ff4j.audit.EventConstants.ACTION_DISCONNECT;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_OFF;
import static org.ff4j.audit.EventConstants.ACTION_TOGGLE_ON;
import static org.ff4j.audit.EventConstants.ACTION_UPDATE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_ACTION;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_HOST;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_ID;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_NAME;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_SOURCE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_TIME;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_TYPE;
import static org.ff4j.audit.EventConstants.ATTRIBUTE_USER;
import static org.ff4j.audit.EventConstants.TARGET_FEATURE;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.bson.Document;
import org.bson.conversions.Bson;
import org.ff4j.audit.EventQueryDefinition;

import com.mongodb.client.model.Accumulators;
import com.mongodb.client.model.Aggregates;
import com.mongodb.client.model.Filters;

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
        return new Document(ATTRIBUTE_ID, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value target value
     * @return event document builder
     */
    public EventDocumentBuilder addEventUuid(String value) {
        builder.append(ATTRIBUTE_ID, value);
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
     * @return event document builder
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
     * @return event document builder
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
     * @return event document builder
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
     * @return event document builder
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
     * @return event document builder
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
     * @return event document builder
     */
    public EventDocumentBuilder addAction(String value) {
        builder.append(ATTRIBUTE_ACTION, value);
        return this;
    }

    public List<Bson> getSelectFeatureUsageFilters(EventQueryDefinition eqd) {
        return buildFilters(eqd, true, false);
    }

    public List<Bson> getSelectAuditTrailFilters(EventQueryDefinition eqd) {
        return buildFilters(eqd, false, true);
    }

    public List<Bson> getPurgeAuditTrailFilters(EventQueryDefinition eqd) {
        return buildFilters(eqd, false, true);
    }

    public List<Bson> getPurgeFeatureUsageFilters(EventQueryDefinition eqd) {
        return buildFilters(eqd, true, false);
    }


    public List<Bson> buildHitCountFilters(EventQueryDefinition qDef, String attr) {
        return Arrays.asList(
                Aggregates.match(Filters.and(Filters.eq(ATTRIBUTE_TYPE, TARGET_FEATURE),
                        Filters.eq(ATTRIBUTE_ACTION, ACTION_CHECK_OK),
                        Filters.gte(ATTRIBUTE_TIME, qDef.getFrom()),
                        Filters.lte(ATTRIBUTE_TIME, qDef.getTo())
                )),
                Aggregates.group("$" + attr, Accumulators.sum("NB", 1)));
    }

    public List<Bson> buildFilters(EventQueryDefinition qDef, boolean filterForCheck, boolean filterAuditTrail) {
        List<Bson> filters = new ArrayList<>();

        filters.add(Filters.gte(ATTRIBUTE_TIME, qDef.getFrom()));
        filters.add(Filters.lte(ATTRIBUTE_TIME, qDef.getTo()));

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
            filters.add(Filters.in(ATTRIBUTE_ACTION, qDef.getActionFilters()));
        }
        if (qDef.getHostFilters() != null && !qDef.getHostFilters().isEmpty()) {
            filters.add(Filters.in(ATTRIBUTE_HOST, qDef.getHostFilters()));
        }
        if (qDef.getNamesFilter() != null && !qDef.getNamesFilter().isEmpty()) {
            filters.add(Filters.in(ATTRIBUTE_NAME, qDef.getNamesFilter()));
        }
        if (qDef.getSourceFilters() != null && !qDef.getSourceFilters().isEmpty()) {
            filters.add(Filters.in(ATTRIBUTE_SOURCE, qDef.getSourceFilters()));
        }

        return filters;
    }

    /**
     * Builder pattern.
     *
     * @return document
     */
    public Document build() {
        return builder;
    }
}
