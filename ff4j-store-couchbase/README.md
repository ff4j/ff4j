# ff4j-store-couchbase

Provides a backend store for FF4J using couchbase. Currently only provides feature & property stores, event repository store is not yet implemented.


### Usage instructions

  - Add dependency, eg.
    ```xml
    <dependency>
        <groupId>org.ff4j</groupId>
        <artifactId>ff4j-store-couchbase</artifactId>
        <version>1.6.6-SNAPSHOT</version>
    </dependency>
    ```
  - Create a bucket for FF4J to use, then create 2 views on the bucket:
    - Design document name: featureDocument, view name: all
        ```javascript
        function (doc, meta) {
          if (doc._class == "org.ff4j.couchbase.store.document.FeatureDocument") {
            emit(meta.id, null);
          }
        }
        ```
    - Design document name: propertyDocument, view name: all
        ```javascript
        function (doc, meta) {
          if (doc._class == "org.ff4j.couchbase.store.document.PropertyDocument") {
            emit(meta.id, null);
          }
        }
        ```
  - Create your couchbase config. Here is an example spring boot config:
    ```java
    @Configuration
    class CouchbaseConfig {
        @Value("${couchbase.cluster.ff4j.ip}")
        private String ff4jIp;
        @Value("${couchbase.cluster.ff4j.bucket}")
        private String ff4jBucketName;
        @Value("${couchbase.cluster.ff4j.password}")
        private String ff4jBucketPassword;

        @Bean
        public Cluster ff4jCluster() {
            return CouchbaseCluster.create(ff4jIp);
        }

        @Bean
        public Bucket ff4jBucket(Cluster ff4jCluster) {
            return ff4jCluster.openBucket(ff4jBucketName, ff4jBucketPassword);
        }
    }
    ```
  - Use in your FF4J config:
    ```java
    @Bean
    public FF4j ff4j() {
        FF4j ff4j = new FF4j();
        ff4j.setFeatureStore(new FeatureStoreCouchbase(ff4jBucket));
        ff4j.setPropertiesStore(new PropertyStoreCouchbase(ff4jBucket));
        return ff4j;
    }
    ```