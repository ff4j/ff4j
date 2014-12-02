---
-- #%L
-- ff4j-store-jdbc
-- %%
-- Copyright (C) 2013 Ff4J
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

INSERT INTO FF4J_FEATURES(FEATURE_NAME, FEATURE_STATUS, DESCRIPTION,REGION_IDENTIFIER) VALUES('AwesomeFeature',  1, 'some desc','dev_d1');

-- First
INSERT INTO FF4J_FEATURES(FEATURE_NAME, FEATURE_STATUS, DESCRIPTION,REGION_IDENTIFIER) VALUES('first',  1, 'description','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('first', 'USER','dev_d1');

-- Second
INSERT INTO FF4J_FEATURES(FEATURE_NAME, FEATURE_STATUS, DESCRIPTION, GROUPNAME,REGION_IDENTIFIER) VALUES('second', 0, 'description', 'GRP0','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('second', 'USER','dev_d1');

-- Third
INSERT INTO FF4J_FEATURES(FEATURE_NAME, FEATURE_STATUS, DESCRIPTION, GROUPNAME,REGION_IDENTIFIER) VALUES('third',  0, 'ThirdJDBC', 'GRP1','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('third', 'ADMINISTRATOR','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('third', 'BETA-TESTER','dev_d1');

-- Forth
INSERT INTO FF4J_FEATURES(FEATURE_NAME, FEATURE_STATUS, DESCRIPTION, STRATEGY, EXPRESSION, GROUPNAME,REGION_IDENTIFIER) 
VALUES('forth',  1, 'ForthJDBC', 'org.ff4j.strategy.el.ExpressionFlipStrategy', 'expression=third|second', 'GRP1','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('forth', 'ADMINISTRATOR','dev_d1');
INSERT INTO FF4J_ROLES(FEATURE_NAME, ROLE_NAME,REGION_IDENTIFIER)  VALUES('forth', 'BETA-TESTER','dev_d1');
