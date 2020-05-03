package org.ff4j.mongo.mapper;

import org.bson.Document;

import java.util.ArrayList;
import java.util.Set;

import static org.ff4j.audit.EventConstants.*;
import static org.ff4j.mongo.MongoDbConstants.*;

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
     * @param value
     *      target value
     * @return
     *      internal mongo object
     */
    public Document getEventUuid(String value) {
        return new Document(EVENT_UUID, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addEventUuid(String value) {
        builder.append(EVENT_UUID, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'name'.
     *
     * @param value
     *      target value
     * @return
     *      internal mong object
     */
    public Document getName(String value) {
        return new Document(ATTRIBUTE_NAME, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addName(String value) {
        builder.append(ATTRIBUTE_NAME, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'host'.
     *
     * @param value
     *      target value
     * @return
     *      internal mongo object
     */
    public Document getHost(String value) {
        return new Document(ATTRIBUTE_HOST, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addHost(String value) {
        builder.append(ATTRIBUTE_HOST, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'user'.
     *
     * @param value
     *      target value
     * @return
     *      internal mongo object
     */
    public Document getUser(String value) {
        return new Document(ATTRIBUTE_USER, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addUser(String value) {
        builder.append(ATTRIBUTE_USER, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'source'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getSource(String value) {
        return new Document(ATTRIBUTE_SOURCE, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addSource(String value) {
        builder.append(ATTRIBUTE_SOURCE, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'type'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getType(String value) {
        return new Document(ATTRIBUTE_TYPE, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addType(String value) {
        builder.append(ATTRIBUTE_TYPE, value);
        return this;
    }

    /**
     * Mongo internal object representing attribute 'action'.
     *
     * @param value
     *            target value
     * @return internal mongo object
     */
    public Document getAction(String value) {
        return new Document(ATTRIBUTE_ACTION, value);
    }

    /**
     * Chain add to build object.
     *
     * @param value
     *            target value
     * @return
     */
    public EventDocumentBuilder addAction(String value) {
        builder.append(ATTRIBUTE_ACTION, value);
        return this;
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
