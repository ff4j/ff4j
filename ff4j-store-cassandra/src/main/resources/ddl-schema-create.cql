-- Keyspace
create keyspace ff4j 
WITH REPLICATION = { 
  'class' : 'SimpleStrategy', 
  'replication_factor' : 3 
};

DROP INDEX IF EXISTS ff4j_features_index_groupname;
DROP TABLE IF EXISTS ff4j_audit;
DROP TABLE IF EXISTS ff4j_properties;
DROP TABLE IF EXISTS ff4j_features;
DROP TYPE  IF EXISTS ff4j_udt_property;
DROP TYPE  IF EXISTS ff4j_udt_strategy;

CREATE TYPE IF NOT EXISTS ff4j_udt_strategy (
 class       text,
 params      map<text,text>
);

CREATE TYPE IF NOT EXISTS ff4j_udt_property (
 uid         text,
 class       text,
 value       text,
 decription  text,
 fixedvalues set<text>
);

CREATE TABLE IF NOT EXISTS ff4j_features (
  groupname    text,
  uid          text,
  enabled      boolean,
  description  text,
  strategy     frozen<ff4j_udt_strategy>,
  roles        set<text>,
  properties   map<text, frozen<ff4j_udt_property>>,
  PRIMARY KEY((uid))
);

CREATE INDEX IF NOT EXISTS ff4j_features_index_groupname  ON ff4j_features (groupname); 

CREATE TABLE IF NOT EXISTS ff4j_properties ( 
 uid         text,
 class       text,
 value       text,
 decription  text,
 fixedvalues set<text>,
 PRIMARY KEY ((uid))
);

