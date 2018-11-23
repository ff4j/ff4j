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
DROP TABLE CONF_SUBMISSION;
DROP TABLE CONFERENCE;
DROP TABLE REF_CONFERENCE;
DROP TABLE REF_COUNTRY;
DROP TABLE REF_REGION;
DROP TABLE REF_MONTH;
DROP TABLE REF_STATUS;
DROP TABLE REF_ADVOCATE;

CREATE TABLE REF_MONTH (
  MONTH varchar(10) PRIMARY KEY,
  NUM   int(2) NOT NULL
);

CREATE TABLE REF_STATUS (
  STATUS varchar(10) PRIMARY KEY
);

CREATE TABLE REF_ADVOCATE (
  ID      varchar(10) PRIMARY KEY,
  FIRSTAME varchar(10) NOT NULL,
  LASTNAME varchar(10) NOT NULL
);

INSERT INTO REF_MONTH (MONTH, NUM) VALUES
('APRIL', 4),
('AUGUST', 8),
('DECEMBER', 12),
('FEBRUARY', 2),
('JANUARY', 1),
('JULY', 7),
('JUNE', 6),
('MARCH', 3),
('MAY', 5),
('NOVEMBER', 11),
('OCTOBER', 10),
('SEPTEMBER', 9);

CREATE TABLE REF_REGION (
  REGION varchar(10) PRIMARY KEY,
  NAME   varchar(50) NOT NULL
);

INSERT INTO REF_REGION (REGION, NAME) VALUES
('APAC', 'Asia Pacific'),
('NA', 'North America'),
('NEMEA', 'North Europe Middle East and Asia'),
('SA', 'South America'),
('SEMEA', 'South Europe Middle East and Asia');

CREATE TABLE REF_COUNTRY (
  NAME varchar(50)  PRIMARY KEY,
  ISO varchar(3)    NOT NULL,
  REGION varchar(10) NOT NULL,
  FOREIGN KEY (REGION) REFERENCES REF_REGION(REGION)
);

CREATE TABLE REF_CONFERENCE (
  UID 			int(4) AUTO_INCREMENT,
  NAME 			varchar(50) NOT NULL,
  COUNTRY 		varchar(30),
  CITY 			varchar(30),
  MONTH 		varchar(10),
  DESCRIPTION 	text,
  URL 			varchar(255),
  TWITTER 		varchar(50),
  ORGANIZER 	varchar(50),
  PRIMARY KEY (UID),
  FOREIGN KEY (MONTH)   REFERENCES REF_MONTH(MONTH),
  FOREIGN KEY (COUNTRY) REFERENCES REF_COUNTRY(NAME)
);

CREATE TABLE CONFERENCE (
  UID 			int(4) AUTO_INCREMENT,
  CONFERENCE    int(4) NOT NULL,
  ATTENDEES     int,
  START			date,
  END			date,
  YEAR          int,
  CFP           int,
  CFP_END       date,
  CFP_URL       varchar(100),
  CITY			varchar(50),
  PRIMARY KEY (UID),
  FOREIGN KEY (CONFERENCE) REFERENCES REF_CONFERENCE(UID)
);

CREATE TABLE CONF_SUBMISSION (
  UID 			int(4) AUTO_INCREMENT,
  ADVOCATE		varchar(10) NOT NULL,
  TITLE         varchar(50) NOT NULL,
  STATUS        varchar(10) NOT NULL,
  CONFERENCE    int(4) NOT NULL,
  PRIMARY KEY (UID),
  FOREIGN KEY (STATUS) REFERENCES REF_STATUS(STATUS),
  FOREIGN KEY (CONFERENCE) REFERENCES CONFERENCE(UID),
  FOREIGN KEY (ADVOCATE) REFERENCES REF_ADVOCATE(ID)
);

INSERT INTO `REF_CONFERENCE` (`UID`, `NAME`, `COUNTRY`, `CITY`, `MONTH`, `DESCRIPTION`, `URL`, `TWITTER`, `ORGANIZER`) VALUES
(1, 'OOP', 'Germany', 'Munich', 'JANUARY', 'The Conference for Software Architecture', 'https://www.oop-konferenz.de/', NULL, NULL);


