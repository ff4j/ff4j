package org.ff4j.couchdb;

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

/**
 * Default settings to connect to couchDB.
 *
 * @author Curtis White (@drizztguen77)
 */
public class CouchDbConstants {

    /**
     * Hide default constructor.
     */
    private CouchDbConstants() { }

    /**
     * Default event name.
     */
    public static final String DEFAULT_EVENT_TYPE = "ff4j_event";

    /**
     * Default event name.
     */
    public static final String DEFAULT_FEATURE_TYPE = "ff4j_feature";

    /**
     * Default event name.
     */
    public static final String DEFAULT_PROPERTY_TYPE = "ff4j_property";

    /**
     * Default mon dg name (use ff4j).
     */
    public static final String DEFAULT_DBNAME = "ff4j";

    /**
     * Default SSL protocol
     */
    public static final String SSL_PROTOCOL = "https";

    /**
     * Default plaintext protocol
     */
    public static final String PLAINTEXT_PROTOCOL = "http";

    /**
     * Default SSL port
     */
    public static final int SSL_PORT = 443;

    /**
     * Default database port
     */
    public static final int DEFAULT_DATABASE_PORT = 5984;
}
