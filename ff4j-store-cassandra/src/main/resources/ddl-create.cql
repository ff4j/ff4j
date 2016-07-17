-- Create keyspace for ff4j
CREATE KEYSPACE IF NOT EXISTS ff4j WITH replication = {'class':'SimpleStrategy', 'replication_factor':3};

USE ff4j;

-- Column Family features
CREATE TABLE IF NOT EXISTS features (
  FEAT_UID          text PRIMARY KEY,
  ENABLE            boolean,
  DESCRIPTION       text,
  STRATEGY          text,
  EXPRESSION        text,
  GROUPNAME         text,
  ROLES             set<text>,
  CUSTOM_PROPERTIES map<text, uuid>
);

-- Speed up query
CREATE INDEX groupName ON features (GROUPNAME);

CREATE TABLE IF NOT EXISTS customProperties (
  UID          UUID PRIMARY KEY,
  PROPERTY_ID  text,
  CLAZZ        text,
  CURRENTVALUE text,
  FIXEDVALUES  text,
  DESCRIPTION  text,
  FEAT_UID     text
);

CREATE TABLE IF NOT EXISTS ff4j.properties (
  PROPERTY_ID  text PRIMARY KEY,
  CLAZZ        text,
  CURRENTVALUE text,
  FIXEDVALUES  text,
  DESCRIPTION  text,
);

CREATE TABLE IF NOT EXISTS ff4j.audit (
  EVT_UUID     uuid PRIMAY KEY,
  EVT_TIME     int,
  EVT_TYPE     text,
  EVT_NAME     text,
  EVT_ACTION   text,
  EVT_HOSTNAME text,
  EVT_SOURCE   text,
  EVT_DURATION int,
  EVT_USER     text,
  EVT_VALUE    text,
  EVT_KEYS     map<text, text>
);



CREATE TABLE IF NOT EXISTS ff4j.feature (
            "id uuid PRIMARY KEY," + 
            "title text," + 
            "album text," + 
            "artist text," + 
            "tags set<text>," + 
            "data blob" + 
            ");
      session.execute(
            "CREATE TABLE IF NOT EXISTS simplex.songs (" +
                  "id uuid PRIMARY KEY," + 
                  "title text," + 
                  "album text," + 
                  "artist text," + 
                  "tags set<text>," + 
                  "data blob" + 
                  ");");
      session.execute(
            "CREATE TABLE IF NOT EXISTS simplex.playlists (" +
                  "id uuid," +
                  "title text," +
                  "album text, " + 
                  "artist text," +
                  "song_id uuid," +
                  "PRIMARY KEY (id, title, album, artist)" +
                  ");");
                  