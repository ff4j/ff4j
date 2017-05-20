# HBASE STORE

Structure of the DB
```shell
hbase(main):008:0> list
TABLE
ff4j_features
ff4j_properties
2 row(s) in 0.0100 seconds
=> ["ff4j_features", "ff4j_properties"]
```

Structure of properties
```sh
hbase(main):021:0> describe 'ff4j_properties'
Table ff4j_properties is ENABLED
ff4j_properties
COLUMN FAMILIES DESCRIPTION
{NAME => 'p', BLOOMFILTER => 'ROW', VERSIONS => '1', IN_MEMORY => 'false', KEEP_DELETED_CELLS => 'FALSE', DATA_BLOCK_ENCO
DING => 'NONE', TTL => 'FOREVER', COMPRESSION => 'NONE', MIN_VERSIONS => '0', BLOCKCACHE => 'true', BLOCKSIZE => '65536',
 REPLICATION_SCOPE => '0'}
1 row(s) in 0.0170 seconds
```

List a rows in properties
```
hbase(main):020:0> count 'ff4j_properties', INTERVAL => 1
Current count: 1, row: a
Current count: 2, row: addPropertyOKsimple
Current count: 3, row: b
Current count: 4, row: c
Current count: 5, row: d
Current count: 6, row: ddate
Current count: 7, row: e
Current count: 8, row: f
Current count: 9, row: g
Current count: 10, row: log
Current count: 11, row: logX
Current count: 12, row: logi
Current count: 13, row: readOKFixed
Current count: 14, row: toto
Current count: 15, row: updateKOInvalidValue
Current count: 16, row: updateOK
16 row(s) in 0.1600 seconds
```

Structure propoerty
```
hbase(main):023:0> get 'ff4j_properties', 'a'
COLUMN                          CELL
 p:CLAZZ                        timestamp=1495305004143, value=org.ff4j.property.PropertyString
 p:DESCRIPTION                  timestamp=1495305004143, value=
 p:FIXEDVALUES                  timestamp=1495305004143, value=["AMER","EAST"]
 p:UID                          timestamp=1495305004143, value=a
 p:VALUE                        timestamp=1495305004143, value=AMER
1 row(s) in 0.0470 seconds
```