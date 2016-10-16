package org.ff4j.cassandra;

/*
 * #%L
 * ff4j-store-cassandra
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
 * Constants to work with 
 *
 * @author Cedrick Lunven (@clunven)
 */
public class CassandraConstants {
    
    /** Keyspace is equivalent to Database in Cassandra. */
    public static final String KEYSPACE = "FF4J";
    
    /** Default cassandra parameter. */
    public static final int PORT = 7000;
    
    /** Default cassandra parameter. */
    public static final int PORT_TLS = 7001;
    
    /** Default cassandra parameter. */
    public static final int PORT_JMX = 7199;
    
    /** Default cassandra parameter. */
    public static final int PORT_THRIFT = 9160 ;
    
    /** Default cassandra parameter. */
    public static final int PORT_CQL_NATIVE = 9042;
    
    /** Default cassandra parameter. */
    public static final String ADMIN_LOGIN = "cassandra";
    
    /** Default cassandra parameter. */
    public static final String ADMIN_PWD = "cassandra";
    
    /** Default cassandra parameter. */
    public static final String DEFAULT_HOST = "127.0.0.1";
    
    /** Default keySpace. */
    public static final String DEFAULT_KEYSPACE = "ff4j";
    
    /** Default keySpace. */
    public static final int DEFAULT_REPLICATION_FACTOR = 3;
    
    // ------- AUDIT -------------

    /** column Family. */
    public static final String COLUMN_FAMILY_AUDIT = "audit";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_UID = "UID";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TIME = "TIME";
    
    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DATE = "DATE";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_TYPE = "TYPE";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_NAME = "NAME";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_ACTION = "ACTION";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_HOSTNAME = "HOSTNAME";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_SOURCE = "SOURCE";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_DURATION = "DURATION";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_USER = "USER";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_VALUE = "VALUE";

    /** sql column name for table FF4J_AUDIT. */
    public static final String COL_EVENT_KEYS = "KEYS";
    
    // -----  Features ---------
    
    /** column Family. */
    public static final String COLUMN_FAMILY_FEATURES = "features";
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_UID = "UID";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ENABLE = "ENABLE";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_DESCRIPTION = "DESCRIPTION";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_GROUPNAME = "GROUPNAME";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_STRATEGY = "STRATEGY";

    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_EXPRESSION = "EXPRESSION";
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_ROLES = "ROLES";
    
    /** sql column name from table FF4J_FEATURES. */
    public static final String COL_FEAT_CUSTOMPROPERTIES = "PROPERTIES";
        
    // ----- Property ------------------
    
    /** column Family. */
    public static final String COLUMN_FAMILY_PROPERTIES = "properties";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_ID = "UID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_CLAZZ = "CLAZZ";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_VALUE = "VALUE";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FIXED = "FIXEDVALUES";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_FEATID = "FEAT_UID";
    
    /** sql column name from table FF4J_PROPERTIES. */
    public static final String COL_PROPERTY_DESCRIPTION = "DESCRIPTION";
    
    // ------- Keyspace ------------------------
    
    /** CQL to work with features. */
    public static final String CQL_CREATEKEYSPACE = "CREATE KEYSPACE IF NOT EXISTS {0} "
            + "WITH replication = '{' '''class''': '''SimpleStrategy''', '''replication_factor''' : {1} '}'";
    
    
    /** Remove public constructor. */
    private CassandraConstants() {
    }
}
