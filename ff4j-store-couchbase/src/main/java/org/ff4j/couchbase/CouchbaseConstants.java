package org.ff4j.couchbase;

/*
 * #%L
 * ff4j-store-couchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.audit.Event;

/**
 * Default settings to connect to couch.
 *
 * @author farrellyja
 * @author Cedrick LUNVEN (@clunven)
 */
public class CouchbaseConstants {

    /**
     * Hide default constructor.
     */
    private CouchbaseConstants() { }
   
    /** Default cassandra parameter. */
    public static final int WEB_ADMIN           = 8091;
    
    /** Default cassandra parameter. */
    public static final int WEB_API             = 8092;
    
    /** Default cassandra parameter. */
    public static final int BUCKET_EXTERNAL     = 11210;
    
    /** Default cassandra parameter. */
    public static final int BUCKET_EXTERNAL_SSL = 11207;
    
    /** Default cassandra parameter. */
    public static final int BUCKET_INTERNAL     = 11209;
    
    /** Default cassandra parameter. */
    public static final int SSL_PROXY            = 11214;
    
    /** Features. */
    public static final String DEFAULT_FEATURE_BUCKETNAME = "ff4jFeatures";
    
    /** Properties. */
    public static final String DEFAULT_PROPERTY_BUCKETNAME = "ff4jProperties";
    
    /** Audit. */
    public static final String DEFAULT_AUDIT_BUCKETNAME    = "ff4jAudit";
    public static final String DEFAULT_AUDIT_VIEWTNAME     = Event.class.getCanonicalName();
    
}
