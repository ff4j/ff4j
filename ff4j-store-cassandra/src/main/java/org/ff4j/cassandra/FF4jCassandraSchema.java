package org.ff4j.cassandra;

/*-
 * #%L
 * ff4j-store-cassandra
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

import static com.datastax.oss.driver.api.core.type.DataTypes.BOOLEAN;
import static com.datastax.oss.driver.api.core.type.DataTypes.INT;
import static com.datastax.oss.driver.api.core.type.DataTypes.TEXT;
import static com.datastax.oss.driver.api.core.type.DataTypes.TIMESTAMP;
import static com.datastax.oss.driver.api.core.type.DataTypes.UUID;
import static com.datastax.oss.driver.api.core.type.DataTypes.mapOf;
import static com.datastax.oss.driver.api.core.type.DataTypes.setOf;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.createTable;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.createIndex;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.createType;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.dropTable;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.dropType;
import static com.datastax.oss.driver.api.querybuilder.SchemaBuilder.udt;

import org.ff4j.utils.Util;

import static com.datastax.oss.driver.api.querybuilder.QueryBuilder.*;

import com.datastax.oss.driver.api.core.CqlSession;
import com.datastax.oss.driver.api.core.cql.SimpleStatement;
import com.datastax.oss.driver.api.querybuilder.QueryBuilder;

/**
 * Grouping of constants for Cassandra model.
 */
public interface FF4jCassandraSchema {

    /** Constant. */
    String DEFAULT_KEYSPACE           = "ff4j";

    /** Constant. */
    String UDT_STRATEGY               = "ff4j_udt_strategy";
    /** Constant. */
    String UDT_STRATEGY_CLASS         = "class";
    /** Constant. */
    String UDT_STRATEGY_PARAMS        = "params";

    /** Constant. */
    String UDT_PROPERTY               = "ff4j_udt_property";
    /** Constant. */
    String UDT_PROPERTY_UID           = "uid";
    /** Constant. */
    String UDT_PROPERTY_CLASS         = "class";
    /** Constant. */
    String UDT_PROPERTY_VALUE         = "value";
    /** Constant. */
    String UDT_PROPERTY_DESCRIPTION   = "description";
    /** Constant. */
    String UDT_PROPERTY_FIXEDVALUES   = "fixedvalues";

    /** Table Features. */
    String FEATURES_TABLE             = "ff4j_features";
    String FEATURES_INDEX_GROUPNAME   = "ff4j_features_index_groupname";
    String FEATURES_ATT_GROUPNAME     = "groupname";
    String FEATURES_ATT_UID           = "uid";
    String FEATURES_ATT_ENABLED       = "enabled";
    String FEATURES_ATT_DESCRIPTION   = "description";
    String FEATURES_ATT_STRATEGY      = "strategy";
    String FEATURES_ATT_ROLES         = "roles";
    String FEATURES_ATT_PROPERTIES    = "properties";

    /** Table Properties. */
    String PROPERTIES_TABLE           = "ff4j_properties";
    String PROPERTIES_ATT_UID         = "uid";
    String PROPERTIES_ATT_CLASS       = "class";
    String PROPERTIES_ATT_VALUE       = "value";
    String PROPERTIES_ATT_DESCRIPTION = "description";
    String PROPERTIES_ATT_FIXEDVALUES = "fixedvalues";

    /** Tables audit. */
    String AUDIT_TABLE                = "ff4j_audit";
    String AUDIT_HITCOUNT_TABLE       = "ff4j_audit_hitcount";
    String AUDIT_ATT_UID              = "uid";
    String AUDIT_ATT_TIME             = "time";
    String AUDIT_ATT_TYPE             = "type";
    String AUDIT_ATT_NAME             = "name";
    String AUDIT_ATT_ACTION           = "action";
    String AUDIT_ATT_HOSTNAME         = "hostname";
    String AUDIT_ATT_SOURCE           = "source";
    String AUDIT_ATT_DURATION         = "duration";
    String AUDIT_ATT_USER             = "user";
    String AUDIT_ATT_VALUE            = "value";
    String AUDIT_ATT_CUSTOM           = "custom";

    /**
     * Getting value for a feature is by id.
     *
     * CREATE TABLE IF NOT EXISTS ff4j_features (
     *   uid          text,
     *   groupname    text,
     *   enabled      boolean,
     *   description  text,
     *   strategy     frozen<ff4j_udt_strategy>,
     *   roles        set<text>,
     *   properties   map<text, frozen<ff4j_udt_property>>,
     *   PRIMARY KEY((uid))
     * );
     */
    SimpleStatement STMT_CREATE_TABLE_FEATURE =
            createTable(FEATURES_TABLE).ifNotExists()
                    .withPartitionKey(FEATURES_ATT_UID, TEXT)
                    .withColumn(FEATURES_ATT_GROUPNAME, TEXT)
                    .withColumn(FEATURES_ATT_ENABLED, BOOLEAN)
                    .withColumn(FEATURES_ATT_DESCRIPTION, TEXT)
                    .withColumn(FEATURES_ATT_STRATEGY, udt(UDT_STRATEGY, true))
                    .withColumn(FEATURES_ATT_ROLES, setOf(TEXT))
                    .withColumn(FEATURES_ATT_PROPERTIES, mapOf(TEXT, udt(UDT_PROPERTY, true)))
                    .build();

    /**
     * CREATE TYPE IF NOT EXISTS ff4j_udt_strategy (
     *  class       text,
     *  params      map<text,text>
     * );
     */
    SimpleStatement STMT_CREATE_UDT_STRATEGY =
            createType(UDT_STRATEGY).ifNotExists()
            .withField(UDT_STRATEGY_CLASS, TEXT)
            .withField(UDT_STRATEGY_PARAMS, mapOf(TEXT, TEXT))
            .build();

    /**
     * Better than allow filtering, and good enough as only used in the admin UI.
     *
     * CREATE INDEX IF NOT EXISTS ff4j_features_index_groupname
     * ON ff4j_features (groupname);
     */
    SimpleStatement STMT_CREATE_INDEX_FEATUREGROUP =
            createIndex(FEATURES_INDEX_GROUPNAME).ifNotExists()
            .onTable(FEATURES_TABLE).andColumn(FEATURES_ATT_GROUPNAME)
            .build();

    /**
     * CREATE TABLE IF NOT EXISTS ff4j_properties (
     *  uid         text,
     *  class       text,
     *  value       text,
     *  decription  text,
     *  fixedvalues set<text>,
     *  PRIMARY KEY ((uid))
     * );
     */
    SimpleStatement STMT_CREATE_TABLE_PROPERTY =
            createTable(PROPERTIES_TABLE).ifNotExists()
            .withPartitionKey(PROPERTIES_ATT_UID, TEXT)
            .withColumn(PROPERTIES_ATT_CLASS, TEXT)
            .withColumn(PROPERTIES_ATT_VALUE, TEXT)
            .withColumn(PROPERTIES_ATT_DESCRIPTION, TEXT)
            .withColumn(PROPERTIES_ATT_FIXEDVALUES, setOf(TEXT))
            .build();

    /**
     * CREATE TYPE IF NOT EXISTS ff4j_udt_property (
     *  uid         text,
     *  class       text,
     *  value       text,
     *  decription  text,
     *  fixedvalues set<text>
     * );
     */
    SimpleStatement STMT_CREATE_UDT_PROPERTY =
            createType(UDT_PROPERTY).ifNotExists()
                    .withField(UDT_PROPERTY_UID, TEXT)
                    .withField(UDT_PROPERTY_CLASS, TEXT)
                    .withField(UDT_PROPERTY_VALUE, TEXT)
                    .withField(UDT_PROPERTY_DESCRIPTION, TEXT)
                    .withField(UDT_PROPERTY_FIXEDVALUES, setOf(TEXT))
                    .build();

    /**
     * CREATE TABLE IF NOT EXISTS ff4j_audit (
     *  uid         uuid,
     *  time        timestamp,
     *  type        text,
     *  name        text,
     *  action      text,
     *  source      text,
     *  hostname    text,
     *  duration    int,
     *  user        text,
     *  value       text,
     *  custom      map<text,text>
     *  PRIMARY KEY ((uid))
     * );
     */
    SimpleStatement STMT_CREATE_TABLE_AUDIT =
            createTable(AUDIT_TABLE).ifNotExists()
            .withPartitionKey(AUDIT_ATT_UID, UUID)
            .withColumn(AUDIT_ATT_TIME, TIMESTAMP)
            .withColumn(AUDIT_ATT_TYPE, TEXT)
            .withColumn(AUDIT_ATT_NAME, TEXT)
            .withColumn(AUDIT_ATT_ACTION, TEXT)
            .withColumn(AUDIT_ATT_SOURCE, TEXT)
            .withColumn(AUDIT_ATT_HOSTNAME, TEXT)
            .withColumn(AUDIT_ATT_DURATION, INT)
            .withColumn(AUDIT_ATT_USER, TEXT)
            .withColumn(AUDIT_ATT_VALUE, TEXT)
            .withColumn(AUDIT_ATT_CUSTOM, mapOf(TEXT,TEXT))
            .build();

    /**
     * CREATE TABLE IF NOT EXISTS ff4j_audit_hitcount (
     *   name       text,
     *   time       timestamp,
     *   uid        uuid,
     *   source     text,
     *   hostname   text,
     *   duration   int,
     *   user       text,
     *   custom      map<text,text>
     *   PRIMARY KEY ((name), time)
     */
    SimpleStatement STMT_CREATE_TABLE_AUDITHITCOUNT =
            createTable(AUDIT_HITCOUNT_TABLE).ifNotExists()
            .withPartitionKey(AUDIT_ATT_NAME, TEXT)
            .withClusteringColumn(AUDIT_ATT_TIME, TIMESTAMP)
            .withColumn(AUDIT_ATT_UID, UUID)
            .withColumn(AUDIT_ATT_SOURCE, TEXT)
            .withColumn(AUDIT_ATT_HOSTNAME, TEXT)
            .withColumn(AUDIT_ATT_DURATION, INT)
            .withColumn(AUDIT_ATT_USER, TEXT)
            .withColumn(AUDIT_ATT_VALUE, TEXT)
            .withColumn(AUDIT_ATT_CUSTOM, mapOf(TEXT,TEXT))
            .build();

    // -- Features --

    SimpleStatement STMT_FEATURE_EXIST =
            selectFrom(FEATURES_TABLE).column(FEATURES_ATT_UID)
            .whereColumn(FEATURES_ATT_UID)
            .isEqualTo(QueryBuilder.bindMarker())
            .build();

    SimpleStatement STMT_FEATURE_READ =
            selectFrom(FEATURES_TABLE).all()
            .whereColumn(FEATURES_ATT_UID)
            .isEqualTo(QueryBuilder.bindMarker())
            .build();

    SimpleStatement STMT_FEATURE_READ_ALL =
            selectFrom(FEATURES_TABLE).all()
            .build();

    SimpleStatement STMT_FEATURE_TOGGLE =
            update(FEATURES_TABLE)
            .setColumn(FEATURES_ATT_ENABLED, bindMarker())
            .whereColumn(FEATURES_ATT_UID).isEqualTo(bindMarker())
            .build();

    SimpleStatement STMT_FEATURE_INSERT =
            insertInto(FEATURES_TABLE)
            .value(FEATURES_ATT_UID,         bindMarker(FEATURES_ATT_UID))
            .value(FEATURES_ATT_GROUPNAME,   bindMarker(FEATURES_ATT_GROUPNAME))
            .value(FEATURES_ATT_ENABLED,     bindMarker(FEATURES_ATT_ENABLED))
            .value(FEATURES_ATT_DESCRIPTION, bindMarker(FEATURES_ATT_DESCRIPTION))
            .value(FEATURES_ATT_ROLES,       bindMarker(FEATURES_ATT_ROLES))
            .value(FEATURES_ATT_STRATEGY,    bindMarker(FEATURES_ATT_STRATEGY))
            .value(FEATURES_ATT_PROPERTIES,  bindMarker(FEATURES_ATT_PROPERTIES))
            .build();

    SimpleStatement STMT_FEATURE_DELETE =
            deleteFrom(FEATURES_TABLE)
            .whereColumn(FEATURES_ATT_UID).isEqualTo(bindMarker())
            .build();

    SimpleStatement STMT_FEATURE_ADDTOGROUP =
            update(FEATURES_TABLE)
            .setColumn(FEATURES_ATT_GROUPNAME, bindMarker())
            .whereColumn(FEATURES_ATT_UID).isEqualTo(bindMarker())
            .build();

    SimpleStatement STMT_FEATURE_REMOVEGROUP =
            deleteFrom(FEATURES_TABLE)
            .column(FEATURES_ATT_GROUPNAME)
            .whereColumn(FEATURES_ATT_UID).isEqualTo(bindMarker())
            .build();

    // Secondary index on groupname make sense here
    SimpleStatement STMT_FEATUREGROUP_READ =
            selectFrom(FEATURES_TABLE).all()
            .whereColumn(FEATURES_ATT_GROUPNAME).isEqualTo(bindMarker())
            //.allowFiltering()
            .build();

    SimpleStatement STMT_FEATUREGROUP_LIST =
            selectFrom(FEATURES_TABLE)
            .column(FEATURES_ATT_GROUPNAME)
            .build();

    // -- Properties --

    SimpleStatement STMT_PROPERTY_EXIST =
            selectFrom(PROPERTIES_TABLE).column(PROPERTIES_ATT_UID)
            .whereColumn(PROPERTIES_ATT_UID)
            .isEqualTo(QueryBuilder.bindMarker())
            .build();

    SimpleStatement STMT_PROPERTY_READ =
            selectFrom(PROPERTIES_TABLE).all()
            .whereColumn(PROPERTIES_ATT_UID)
            .isEqualTo(QueryBuilder.bindMarker())
            .build();

    SimpleStatement STMT_PROPERTY_READ_ALL =
            selectFrom(PROPERTIES_TABLE).all()
            .build();

    SimpleStatement STMT_PROPERTY_LISTNAMES =
            selectFrom(PROPERTIES_TABLE).column(PROPERTIES_ATT_UID)
            .build();

    SimpleStatement STMT_PROPERTY_DELETE =
            deleteFrom(PROPERTIES_TABLE)
            .whereColumn(PROPERTIES_ATT_UID).isEqualTo(bindMarker())
            .build();

    SimpleStatement STMT_PROPERTY_INSERT =
            insertInto(PROPERTIES_TABLE)
            .value(PROPERTIES_ATT_UID,         bindMarker(PROPERTIES_ATT_UID))
            .value(PROPERTIES_ATT_CLASS,       bindMarker(PROPERTIES_ATT_CLASS))
            .value(PROPERTIES_ATT_VALUE,       bindMarker(PROPERTIES_ATT_VALUE))
            .value(PROPERTIES_ATT_DESCRIPTION, bindMarker(PROPERTIES_ATT_DESCRIPTION))
            .value(PROPERTIES_ATT_FIXEDVALUES, bindMarker(PROPERTIES_ATT_FIXEDVALUES))
            .build();

    // SMTT

    SimpleStatement STMT_AUDIT_INSERT =
            insertInto(AUDIT_TABLE)
            .value(AUDIT_ATT_TYPE,   bindMarker(AUDIT_ATT_TYPE))
            .value(AUDIT_ATT_TIME,   bindMarker(AUDIT_ATT_TIME))
            .value(AUDIT_ATT_UID,    bindMarker(AUDIT_ATT_UID))
            .value(AUDIT_ATT_NAME,   bindMarker(AUDIT_ATT_NAME))
            .value(AUDIT_ATT_ACTION, bindMarker(AUDIT_ATT_ACTION))
            .value(AUDIT_ATT_SOURCE, bindMarker(AUDIT_ATT_SOURCE))
            .value(AUDIT_ATT_HOSTNAME, bindMarker(AUDIT_ATT_HOSTNAME))
            .value(AUDIT_ATT_DURATION, bindMarker(AUDIT_ATT_DURATION))
            .value(AUDIT_ATT_USER,   bindMarker(AUDIT_ATT_USER))
            .value(AUDIT_ATT_VALUE,  bindMarker(AUDIT_ATT_VALUE))
            .value(AUDIT_ATT_CUSTOM, bindMarker(AUDIT_ATT_CUSTOM))
            .build();
    SimpleStatement STMT_AUDIT_INSERT_HITCOUNT =
            insertInto(AUDIT_HITCOUNT_TABLE)
            .value(AUDIT_ATT_UID,    bindMarker(AUDIT_ATT_UID))
            .value(AUDIT_ATT_TIME,   bindMarker(AUDIT_ATT_TIME))
            .value(AUDIT_ATT_NAME,   bindMarker(AUDIT_ATT_NAME))
            .value(AUDIT_ATT_SOURCE, bindMarker(AUDIT_ATT_SOURCE))
            .value(AUDIT_ATT_HOSTNAME, bindMarker(AUDIT_ATT_HOSTNAME))
            .value(AUDIT_ATT_DURATION, bindMarker(AUDIT_ATT_DURATION))
            .value(AUDIT_ATT_USER,   bindMarker(AUDIT_ATT_USER))
            .value(AUDIT_ATT_VALUE,  bindMarker(AUDIT_ATT_VALUE))
            .value(AUDIT_ATT_CUSTOM, bindMarker(AUDIT_ATT_CUSTOM))
            .build();

    SimpleStatement STMT_AUDIT_READ_BY_ID = selectFrom(AUDIT_TABLE)
            .all()
            .whereColumn(AUDIT_ATT_UID)
            .isEqualTo(QueryBuilder.bindMarker())
            .build();


    /**
     * Drop a table.
     *
     * @param session
     *      current session
     * @param tableName
     *      table name
     */
    default void dropTableIfExists(CqlSession session, String tableName) {
        Util.assertNotNull(session);
        Util.assertHasLength(tableName);
        session.execute(dropTable(tableName).ifExists().build());
    }

    /**
     * Drop a type.
     *
     * @param session
     *      current session
     * @param typeName
     *      type name
     */
    default void dropTypeIffExists(CqlSession session, String typeName) {
        Util.assertNotNull(session);
        Util.assertHasLength(typeName);
        session.execute(dropType(typeName).ifExists().build());
    }

    default void truncateTable(CqlSession session, String tableName) {
        session.execute(truncate(tableName).build());
    }

    default boolean isTableExist(CqlSession cqlSession, String tableName) {
        Util.assertNotNull(cqlSession);
        Util.assertHasLength(tableName);
        return cqlSession.getMetadata()
                         .getKeyspace(cqlSession.getKeyspace().get())
                         .get().getTable(tableName)
                         .isPresent();
    }


}
