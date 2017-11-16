package org.ff4j.test.store;

/*
 * #%L
 * ff4j-store-springcouchbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
 * %%
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 *      http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 * #L%
 */

import com.couchbase.client.java.Bucket;
import com.couchbase.client.java.Cluster;
import com.couchbase.client.java.CouchbaseCluster;
import com.fasterxml.jackson.databind.ObjectMapper;
import org.ff4j.couchbase.store.document.PropertyDocument;
import org.ff4j.couchbase.store.mapper.ObjectMapperFactory;
import org.ff4j.couchbase.store.repository.CouchbaseRepository;
import org.ff4j.couchbase.store.store.PropertyStoreCouchbase;
import org.ff4j.property.store.PropertyStore;
import org.junit.Before;

import java.io.IOException;
import java.util.Arrays;

public class PropertyStoreCouchbaseTest extends PropertyStoreTestSupportCouchbase {
    private static final String adminUser = "Administrator";
    private static final String adminPassword = "password";
    private static final String ff4jIp = "localhost";
    private static final int ff4jPort = 8092;
    private static final String ff4jBucketName = "featuresTest";
    private static final String ff4jBucketPassword = "";
    private static boolean mockDataCreated = false;

    private void createMockData(CouchbaseRepository<PropertyDocument> propertyRepository) {
        try {
            ObjectMapper mapper = ObjectMapperFactory.createMapper();
            MockData mockData
                = mapper.readValue(getClass().getResourceAsStream("/mock-data.json"), MockData.class);

            Arrays.stream(mockData.getProperties())
                .forEach(p -> {
                    try {
                        propertyRepository.upsert(p.getName(), p);
                    } catch (Exception e) {
                        throw new RuntimeException("Could not read feature test data", e);
                    }
                });
        } catch (IOException e) {
            throw new RuntimeException("Could not read test data", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        Cluster ff4jCluster = CouchbaseCluster.create(ff4jIp);
        Bucket ff4jBucket;

        if (!mockDataCreated) {
            ff4jBucket = ff4jCluster.openBucket(ff4jBucketName, ff4jBucketPassword);
            ff4jBucket.bucketManager().flush();
            createMockData(
                new CouchbaseRepository<>(ff4jBucket, PropertyDocument.class, ObjectMapperFactory.createMapper())
            );
            mockDataCreated = true;
        } else {
            ff4jBucket = ff4jCluster.openBucket(ff4jBucketName, ff4jBucketPassword);
        }

        return new PropertyStoreCouchbase(ff4jBucket);
    }

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
    }
}
