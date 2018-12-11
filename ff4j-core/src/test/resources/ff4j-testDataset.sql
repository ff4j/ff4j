---
-- #%L
-- ff4j-core
-- %%
-- Copyright (C) 2013 - 2018 FF4J
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
-- f1 --
INSERT INTO FF4J_FEATURE
       (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, ENABLE, GROUPNAME) 
VALUES ('f1', NULL, NULL, NULL, 'some desc', '0', NULL);

-- f2 --
INSERT INTO FF4J_FEATURE 
	   (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, ENABLE, GROUPNAME) 
VALUES ('f2', NULL, NULL, NULL, 'description', '1', 'GRP1');
INSERT INTO FF4J_FEATURE_PERM 
       (FEAT_UID, PERMISSION, USERS, ROLES) 
VALUES ('f2', 'FEATURE_TOGGLE', 'john', NULL), 
       ('f2', 'FEATURE_VIEW', NULL, 'EVERYONE');
INSERT INTO FF4J_FEATURE_PROP 
       (UID, CLASSNAME, VAL, FIXEDVALUES, FEAT_UID) 
VALUES ('ppint', 'int', '12', NULL, 'f2'), 
	   ('ppdouble', 'double', '12.5', NULL, 'f2'),
	   ('ppboolean', 'boolean', 'true', NULL, 'f2'), 
	   ('ppstring', 'string', 'hello', NULL, 'f2'),
	   ('ppListInt', 'listInt', '12,13,14', NULL, 'f2'), 
	   ('myLogLevel', 'logLevel', 'DEBUG', NULL, 'f2'),
	   ('digitValue', 'org.ff4j.property.PropertyInt', '1', '0,1,2,3', 'f2'), 
	   ('regionIdentifier', 'string', 'NA', 'NA,EMEA,APAC', 'f2');
INSERT INTO FF4J_FEATURE_STRAT (FEAT_UID, TOGGLE_CLASS) 
VALUES ('f2', 'org.ff4j.feature.togglestrategy.PonderationToggleStrategy');
INSERT INTO FF4J_FEATURE_STRAT_P
	   (UID, CLASSNAME, VAL, FIXEDVALUES, STRAT_FEAT_UID, STRAT_CLASS) 
VALUES ('weight', 'double', '1', NULL, 'f2', 'org.ff4j.feature.togglestrategy.PonderationToggleStrategy');

-- f3 --
INSERT INTO FF4J_FEATURE 
	   (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, ENABLE, GROUPNAME) 
VALUES ('f3', NULL, NULL, NULL, 'description', '0', 'GRP0');
INSERT INTO FF4J_FEATURE_PERM 
	   (FEAT_UID, PERMISSION, USERS, ROLES) 
VALUES ('f3', 'FEATURE_VIEW', NULL, 'USER');

-- f4 --
INSERT INTO FF4J_FEATURE 
	   (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, ENABLE, GROUPNAME) 
VALUES ('f4', NULL, NULL, NULL, 'description', '1', 'GRP1');
INSERT INTO FF4J_FEATURE_STRAT 
	   (FEAT_UID, TOGGLE_CLASS) 
VALUES ('f4', 'org.ff4j.feature.togglestrategy.expression.ExpressionToggleStrategy');
INSERT INTO FF4J_FEATURE_STRAT_P 
	   (UID, CLASSNAME, VAL, FIXEDVALUES, STRAT_FEAT_UID, STRAT_CLASS) 
VALUES ('expression', 'string', 'f3 | f2', NULL, 'f4', 'org.ff4j.feature.togglestrategy.expression.ExpressionToggleStrategy');

-- Roles --
INSERT INTO FF4J_ROLE (NAME, DESCRIPTION) VALUES ('EVERYONE', NULL);
INSERT INTO FF4J_ROLE (NAME, DESCRIPTION) VALUES ('USER', NULL);
INSERT INTO FF4J_ROLE (NAME, DESCRIPTION) VALUES ('SUPERUSER', NULL);
INSERT INTO FF4J_ROLE (NAME, DESCRIPTION) VALUES ('ADMINISTRATOR', NULL);
INSERT INTO FF4J_ROLE_PERM (ROLE_NAME, PERMISSION)
VALUES ('EVERYONE', 'VIEW_FEATURES'), 
	   ('EVERYONE', 'VIEW_PROPERTIES'),
	   ('USER', 'VIEW_FEATURES'), 
	   ('USER', 'VIEW_PROPERTIES'),
	   ('USER', 'VIEW_AUDITTRAIL'), 
	   ('USER', 'VIEW_FEATUREUSAGE'),
	   ('SUPERUSER', 'TOGGLE_FEATURES'), 
	   ('SUPERUSER', 'VIEW_FEATURES'),
	   ('SUPERUSER', 'VIEW_PROPERTIES'), 
	   ('SUPERUSER', 'VIEW_AUDITTRAIL'),
	   ('SUPERUSER', 'VIEW_FEATUREUSAGE'),
	   ('ADMINISTRATOR', 'ADMIN_FEATURES'),
	   ('ADMINISTRATOR', 'ADMIN_PROPERTIES'),
	   ('ADMINISTRATOR', 'TOGGLE_FEATURES'), 
	   ('ADMINISTRATOR', 'VIEW_FEATURES'),
	   ('ADMINISTRATOR', 'VIEW_PROPERTIES'), 
	   ('ADMINISTRATOR', 'VIEW_AUDITTRAIL'),
	   ('ADMINISTRATOR', 'VIEW_FEATUREUSAGE');
	  
-- Users --	   
INSERT INTO FF4J_USER (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, PASSWORD, LASTNAME, FIRSTNAME) 
VALUES ('john', NULL, NULL, NULL, 'sample description if OK', 'john', 'Connor', 'John');
INSERT INTO FF4J_USER (UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, PASSWORD, LASTNAME, FIRSTNAME) 
VALUES ('sarah', NULL, NULL, NULL, 'sample description if OK', '', 'Connor', 'Sarah');
INSERT INTO FF4J_USER_PERM (USER_UID, PERMISSION) 
VALUES ('john', 'ADMIN_FEATURES'),
	   ('sarah', 'FEATURE_VIEW');
INSERT INTO FF4J_USER_ROLE_A (REF_USER, REF_ROLE) 
VALUES ('john', 'ADMINISTRATOR'), 
	   ('sarah', 'USER');

-- Properties --
INSERT INTO FF4J_PROPERTY 
		(UID, CREATED, LASTMODIFIED, OWNER, DESCRIPTION, CLASSNAME, VAL, FIXEDVALUES) 
VALUES ('pBigDecimal', NULL, NULL, NULL, NULL, 'bigDecimal', '1.5', NULL), 
	   ('pBigInteger', NULL, NULL, NULL, NULL, 'bigInteger',  '123456', NULL),
	   ('pBoolean', NULL, NULL, NULL, NULL, 'boolean',  'true', NULL), 
	   ('pByte', NULL, NULL, NULL, NULL, 'byte',  'p', NULL),
	   ('pCalendar', NULL, NULL, NULL, NULL, 'calendar',  '2018-12-24 23:00:00', NULL), 
	   ('pDate', NULL, NULL, NULL, NULL, 'date',  '2018-12-24 23:00:00', NULL),
	   ('pClass', NULL, NULL, NULL, NULL, 'class',  'java.lang.String', NULL), 
	   ('pDouble', NULL, NULL, NULL, NULL, 'double',  '20.0', NULL),
	   ('pFloat', NULL, NULL, NULL, NULL, 'float',  '20.0', NULL), 
	   ('pInstant', NULL, NULL, NULL, NULL, 'instant',  '2018-12-24 23:00:00', NULL),
	   ('pLocal', NULL, NULL, NULL, NULL, 'localDateTime', '2018-12-24 23:00:00', NULL), 
	   ('pInt', NULL, NULL, NULL, NULL, 'int',  '10', NULL),

	   ('pLogLevel', NULL, NULL, NULL, NULL, 'logLevel',  'INFO', NULL), 
	   ('pLong', NULL, NULL, NULL, NULL, 'long',  '123', NULL),
	   ('pShort', NULL, NULL, NULL, NULL, 'short',  '5', NULL), 
	   ('pString', NULL, NULL, NULL, NULL, 'string', 'pString', NULL),
	   ('pListBigDecimal', NULL, NULL, NULL, NULL, 'listBigDecimal',  '1.5,2.5', NULL), 
	   ('pListBigInteger', NULL, NULL, NULL, NULL, 'listBigInteger',  '123456,789012', NULL),
	   ('pListBoolean', NULL, NULL, NULL, NULL, 'listBoolean', 'true,false', NULL), 
	   ('pListByte', NULL, NULL, NULL, NULL, 'listByte',  'p,l', NULL),
	   ('pListCalendar', NULL, NULL, NULL, NULL, 'listCalendar', '2018-12-24 23:00:00,2019-01-01 23:00:00', NULL), 
	   ('pListDate', NULL, NULL, NULL, NULL, 'listDate',  '2018-12-24 23:00:00,2019-01-01 23:00:00', NULL),
	   ('pListClass', NULL, NULL, NULL, NULL, 'listClass',  'java.lang.String,java.lang.Integer', NULL), 
	   ('pListDouble', NULL, NULL, NULL, NULL, 'listDouble',  '20.0,30.0', NULL),
	   ('pListFloat', NULL, NULL, NULL, NULL, 'listFloat',  '20.0,30.0', NULL), 
	   ('plistInstant', NULL, NULL, NULL, NULL, 'listInstant',  '2018-12-24 23:00:00,2019-01-01 23:00:00', NULL),
	   ('pListInt', NULL, NULL, NULL, NULL, 'listInt',  '10,20', NULL), 
	   ('pListLogLevel', NULL, NULL, NULL, NULL, 'listLogLevel',  'INFO,DEBUG', NULL),
	   ('pListLong', NULL, NULL, NULL, NULL, 'listLong',  '123,456', NULL), 
	   ('pListShort', NULL, NULL, NULL, NULL, 'listShort',  '5,6', NULL),
	   ('pListString', NULL, NULL, NULL, NULL, 'listString',  'pString,listString', NULL), 
	   ('pListLocal', NULL, NULL, NULL, NULL, 'listLocalDateTime', '2018-12-24 23:00:00,2019-01-01 23:00:00', NULL);

