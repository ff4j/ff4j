-- #%L
-- ff4j-store-cassandra
-- %%
-- Copyright (C) 2013 - 2020 FF4J
-- %%
-- Licensed under the Apache License, Version 2.0 (the "License");
-- you may not use this file except in compliance with the License.
-- You may obtain a copy of the License at
-- 
--      http://www.apache.org/licenses/LICENSE-2.0
-- 
-- Unless required by applicable law or agreed to in writing, software
-- distributed under the License is distributed on an "AS IS" BASIS,
-- WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
-- See the License for the specific language governing permissions and
-- limitations under the License.
-- #L%
---
CREATE KEYSPACE IF NOT EXISTS ff4j 
WITH REPLICATION = 
 { 'class' : 'SimpleStrategy', 
   'replication_factor': '1' } 
AND DURABLE_WRITES = true;

CREATE TYPE ff4j.ff4j_udt_property (
    uid text,
    class text,
    value text,
    decription text,
    fixedvalues set<text>
);

CREATE TYPE ff4j.ff4j_udt_strategy (
    class text,
    params map<text, text>
);

CREATE TABLE ff4j.ff4j_audit (
    uid uuid,
    action text,
    custom map<text, text>,
    duration int,
    hostname text,
    name text,
    source text,
    time timestamp,
    type text,
    user text,
    value text,
    PRIMARY KEY (uid)
) WITH read_repair_chance = 0.0
    AND dclocal_read_repair_chance = 0.0
    AND gc_grace_seconds = 864000
    AND bloom_filter_fp_chance = 0.01
    AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE' }
    AND comment = ''
    AND compaction = { 'class' : 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold' : 32, 'min_threshold' : 4 }
    AND compression = { 'enabled' : 'false' }
    AND default_time_to_live = 0
    AND speculative_retry = 'NONE'
    AND min_index_interval = 128
    AND max_index_interval = 2048
    AND crc_check_chance = 1.0
    AND cdc = false
    AND memtable_flush_period_in_ms = 0
    AND nodesync = { 'enabled' : 'true', 'incremental' : 'true' };

CREATE TABLE ff4j.ff4j_audit_by_type (
    type text,
    time timestamp,
    uid uuid,
    action text,
    custom map<text, text>,
    duration int,
    hostname text,
    name text,
    source text,
    user text,
    value text,
    PRIMARY KEY (type, time, uid)
) WITH CLUSTERING ORDER BY (time ASC, uid ASC)
    AND read_repair_chance = 0.0
    AND dclocal_read_repair_chance = 0.0
    AND gc_grace_seconds = 864000
    AND bloom_filter_fp_chance = 0.01
    AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE' }
    AND comment = ''
    AND compaction = { 'class' : 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold' : 32, 'min_threshold' : 4 }
    AND compression = { 'enabled' : 'false' }
    AND default_time_to_live = 0
    AND speculative_retry = 'NONE'
    AND min_index_interval = 128
    AND max_index_interval = 2048
    AND crc_check_chance = 1.0
    AND cdc = false
    AND memtable_flush_period_in_ms = 0
    AND nodesync = { 'enabled' : 'true', 'incremental' : 'true' };

CREATE TABLE ff4j.ff4j_audit_hitcount (
    name text,
    time timestamp,
    custom map<text, text>,
    duration int,
    hostname text,
    source text,
    uid uuid,
    user text,
    value text,
    PRIMARY KEY (name, time)
) WITH CLUSTERING ORDER BY (time ASC)
    AND read_repair_chance = 0.0
    AND dclocal_read_repair_chance = 0.0
    AND gc_grace_seconds = 864000
    AND bloom_filter_fp_chance = 0.01
    AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE' }
    AND comment = ''
    AND compaction = { 'class' : 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold' : 32, 'min_threshold' : 4 }
    AND compression = { 'enabled' : 'false' }
    AND default_time_to_live = 0
    AND speculative_retry = 'NONE'
    AND min_index_interval = 128
    AND max_index_interval = 2048
    AND crc_check_chance = 1.0
    AND cdc = false
    AND memtable_flush_period_in_ms = 0
    AND nodesync = { 'enabled' : 'true', 'incremental' : 'true' };

CREATE TABLE ff4j.ff4j_features (
    uid text,
    description text,
    enabled boolean,
    groupname text,
    properties map<text, frozen<ff4j.ff4j_udt_property>>,
    roles set<text>,
    strategy frozen<ff4j.ff4j_udt_strategy>,
    PRIMARY KEY (uid)
) WITH read_repair_chance = 0.0
    AND dclocal_read_repair_chance = 0.0
    AND gc_grace_seconds = 864000
    AND bloom_filter_fp_chance = 0.01
    AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE' }
    AND comment = ''
    AND compaction = { 'class' : 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold' : 32, 'min_threshold' : 4 }
    AND compression = { 'enabled' : 'false' }
    AND default_time_to_live = 0
    AND speculative_retry = 'NONE'
    AND min_index_interval = 128
    AND max_index_interval = 2048
    AND crc_check_chance = 1.0
    AND cdc = false
    AND memtable_flush_period_in_ms = 0
    AND nodesync = { 'enabled' : 'true', 'incremental' : 'true' };

CREATE INDEX ff4j_features_index_groupname ON ff4j.ff4j_features (groupname);

CREATE TABLE ff4j.ff4j_properties (
    uid text,
    class text,
    description text,
    fixedvalues set<text>,
    value text,
    PRIMARY KEY (uid)
) WITH read_repair_chance = 0.0
    AND dclocal_read_repair_chance = 0.0
    AND gc_grace_seconds = 864000
    AND bloom_filter_fp_chance = 0.01
    AND caching = { 'keys' : 'ALL', 'rows_per_partition' : 'NONE' }
    AND comment = ''
    AND compaction = { 'class' : 'org.apache.cassandra.db.compaction.SizeTieredCompactionStrategy', 'max_threshold' : 32, 'min_threshold' : 4 }
    AND compression = { 'enabled' : 'false' }
    AND default_time_to_live = 0
    AND speculative_retry = 'NONE'
    AND min_index_interval = 128
    AND max_index_interval = 2048
    AND crc_check_chance = 1.0
    AND cdc = false
    AND memtable_flush_period_in_ms = 0
    AND nodesync = { 'enabled' : 'true', 'incremental' : 'true' };