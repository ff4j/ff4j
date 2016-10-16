-- Keyspace
create keyspace ff4j WITH REPLICATION = { 'class' : 'SimpleStrategy', 'replication_factor' : 3 };

-- Create table Feature
CREATE TABLE ff4j.features ( 
   UID varchar, 
   ENABLE int, 
   DESCRIPTION varchar, 
   STRATEGY varchar, 
   GROUPNAME varchar, 
   ROLES set<varchar>, 
   PROPERTIES map<varchar,varchar>, 
   PRIMARY KEY (UID)
);

-- create table properties
CREATE TABLE ff4j.properties ( 
   UID varchar, 
   CLAZZ varchar, 
   VALUE varchar, 
   DESCRIPTION varchar,
   FIXEDVALUES set<varchar>, 
   PRIMARY KEY (UID)
);

-- create table audit (group per date)
CREATE TABLE ff4j.audit (
    uid varchar,
    date varchar,
    time timestamp,
    type varchar,
    name varchar,
    action varchar,
    hostName varchar,
    source varchar,
    duration int,
    user varchar,
    value varchar,
    custom map<varchar,varchar>,
    PRIMARY KEY (UID)
);

INSERT INTO ff4j.audit(uid, date, time, type, name, action, hostname, source , duration ,  user , value)
VALUES ('113be6af-0d70-470c-b117-b29a08d99761','2013-04-03', '2013-04-03 07:03:00','feature', 'f1', 'toggle', 'localhost', 'java_api', 12,'admin', 'ok');

INSERT INTO ff4j.audit(uid, date, time, type, name, action, hostname, source , duration ,  user , value)
VALUES ('113be6af-0d70-470c-b117-b29a08d99762','2013-04-03', '2013-04-03 07:03:01','feature', 'f1', 'toggle', 'localhost', 'java_api', 12,'admin', 'ok');

INSERT INTO ff4j.audit(uid, date, time, type, name, action, hostname, source , duration ,  user , value)
VALUES ('113be6af-0d70-470c-b117-b29a08d99763','2013-04-03', '2013-04-03 07:03:02','feature', 'f1', 'toggle', 'localhost', 'java_api', 12,'admin', 'ok');


-- publish Event
select uid from audit where name='f1' and type='feature' and date in('2013-04-03', '2013-04-04');




