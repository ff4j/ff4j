# ff4j-store-couchbase

Provides a backend store for FF4J using couchbase. Currently only provides feature & property stores, (event repository store is not yet implemented).


### Usage instructions

  - Add dependency, eg.
    ```xml
    <dependency>
        <groupId>org.ff4j</groupId>
        <artifactId>ff4j-store-couchbase</artifactId>
        <version>1.6.6-SNAPSHOT</version>
    </dependency>
    ```
  - Create Flushable buckets for FF4j properties and ff4jFeature (defaults expected are `ff4jFeatures` and `ff4jProperties`.
  
  - Create primary indexes for each bucket.
     ```
     CREATE PRIMARY INDEX `ff4jFeatures-index` ON `ff4jFeatures` USING GSI;
     CREATE PRIMARY INDEX `ff4jProperties-index` ON `ff4jProperties` USING GSI;
     
    ```
  - Use in your FF4J config:
    ```java
    @Bean
    public FF4j ff4j() {
         CouchbaseConnection conn = new CouchbaseConnection().addNode("127.0.0.1")
           .userName("Administrator")
           .password("password")
           //[optional].propertyBucketName("ff4jProperties");
           //[optional].featureBucketName("ff4jFeatures");
           //[optional].propertyBucketPassword("password");
           //[optional].featureBucketPassword("password");    
                    
        FF4j ff4j = new FF4j();
        ff4j.setFeatureStore(new FeatureStoreCouchbase(conn));
        ff4j.setPropertiesStore(new PropertyStoreCouchbase(conn));
        return ff4j;
    }
    ```