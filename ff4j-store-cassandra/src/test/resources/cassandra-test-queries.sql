---
-- #%L
-- ff4j-store-cassandra
-- %%
-- Copyright (C) 2013 - 2016 FF4J
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
-- List tables
describe tables;


 -- Afficher une table
 SELECT * FROM ff4j.features;

-- Populate FEATURE
INSERT INTO ff4j.features (FEAT_UID, ENABLE, DESCRIPTION, GROUPNAME, ROLES)
VALUES('f1', 1, 'sample desc', 'groupe1', {'ADMIN', 'USER'});
INSERT INTO ff4j.features (UID, ENABLE, DESCRIPTION, GROUPNAME, ROLES, STRATEGY, PROPERTIES)
VALUES('f2', 1, 'sample desc', 'groupe1', {'ADMIN', 'USER'}, '{"flippingStrategy":"value", "initParam":"ok"}', 
        {'p1':'{"type":"int", "value":3}', 'p2':'v2'});
INSERT INTO ff4j.features (FEAT_UID, ENABLE, CUSTOM_PROPERTIES) VALUES('f3', 1, {'p1':'v1', 'p2':'v2'});
INSERT INTO ff4j.features (FEAT_UID, ENABLE, CUSTOM_PROPERTIES) VALUES('f4', 1, {'p1':'{"type":"int", "value":3}', 'p2':'v2'});

-- Disable
UPDATE ff4j.features SET enable='0' WHERE feat_uid = 'f1';

-- Truncate
CONSISTENCY ALL;TRUNCATE TABLE ff4j.features;

-- Grant
UPDATE users SET emails = emails + {'fb@friendsofmordor.org'} WHERE user_id = 'frodo';
UPDATE features SET roles = roles + {'ooo'} WHERE uid = 'fx1';

-- Empty set
UPDATE users SET emails = {} WHERE user_id = 'frodo';

DELETE emails FROM users WHERE user_id = 'frodo';

CREATE INDEX ON ff4j.features (GROUPNAME);

SELECT * from ff4j.features where GROUPNAME = 'group3';

UPDATE features SET enable = 1 WHERE GROUPNAME = 'group3';

-- requete TIME/FEATURE(name+type)

CREATE TABLE latest_temperatures (
weatherstation_id text,
event_time timestamp,
temperature text,
PRIMARY KEY (weatherstation_id,event_time),
) WITH CLUSTERING ORDER BY (event_time DESC);

INSERT INTO latest_temperatures(weatherstation_id,event_time,temperature)
VALUES (’1234ABCD’,’2013-04-03 07:03:00′,’72F’) USING TTL 20;

ALTER TABLE users ALTER bio TYPE text;
ALTER TABLE cycling.cyclist_races ADD firstname text;
ALTER TABLE cycling.basic_info DROP birth_year;


select * from audit
where time > '2013-04-03 07:01:00'
AND time < '2013-04-03 07:04:00'
