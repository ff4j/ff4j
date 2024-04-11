package org.ff4j.couchdb.store;

/*-
 * #%L
 * ff4j-store-couchdb
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import org.ff4j.property.store.PropertyStore;
import org.junit.Ignore;

/**
 * CouchDb Property Store Tests.
 *
 * @author Curtis White (@drizztguen77)
 */
@Ignore
public class PropertyStoreCouchDbTest {

    /**
     * Reuse connection for tests.

    private static CouchDbConnection conn = null;
    private static CouchDbPropertyView repo = null;
    private static int COUCHDB_PORT = 5984;
    private static String TEST_USER = "ff4j";
    private static String TEST_PASSWORD = "ff4j";
    private static String DB_NAME = "ff4j";

    @ClassRule
    public static GenericContainer<?> couchdb = new GenericContainer<>("couchdb:latest")
            .withExposedPorts(COUCHDB_PORT)
            .withEnv("COUCHDB_USER", TEST_USER)
            .withEnv("COUCHDB_PASSWORD", TEST_PASSWORD)
            .waitingFor(Wait.forListeningPort());
    */

    /**
     * {@inheritDoc}
     */
    protected PropertyStore initPropertyStore() {
        return null;
       /* WaitingConsumer consumer = new WaitingConsumer();
        couchdb.followOutput(consumer, STDOUT, STDERR);

        if (conn == null) {

            conn = new CouchDbConnection()
                    .dbName(DB_NAME)
                    .userName(TEST_USER)
                    .password(TEST_PASSWORD)
                    .host(couchdb.getContainerIpAddress())
                    .port(couchdb.getFirstMappedPort())
                    .createDatabaseIfNotExists(true)
            ;
        }

        CouchDbConnector connector = conn.getCouchDbConnector();

        if (repo == null) {
            repo = new CouchDbPropertyView(connector);
        }

        PropertyStoreCouchDb store = new PropertyStoreCouchDb(conn, repo);
        store.clear();
        store.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return store;*/
    }
}
