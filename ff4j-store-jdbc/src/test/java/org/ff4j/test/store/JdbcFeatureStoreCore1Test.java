package org.ff4j.test.store;

/*
 * #%L
 * ff4j-store-jdbc
 * %%
 * Copyright (C) 2013 Ff4J
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

import org.ff4j.core.FeatureStore;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.test.AbstractStoreTest;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

public class JdbcFeatureStoreCore1Test extends AbstractStoreTest {

    /** DataBase. */
    private EmbeddedDatabase db;

    private EmbeddedDatabaseBuilder builder = null;

    @Override
    protected FeatureStore initStore() throws Exception {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).addScript("classpath:schema-ddl.sql").addScript("classpath:ff-store.sql")
                .build();

        JdbcFeatureStore jdbcStore = new JdbcFeatureStore();
        jdbcStore.setDataSource(db);
        return jdbcStore;
    }

    /** {@inheritDoc} */
    @Override
    protected void setUp() throws Exception {
        super.setUp();
        db = builder.setType(EmbeddedDatabaseType.HSQL).addScript("classpath:schema-ddl.sql").addScript("classpath:ff-store.sql")
                .build();
    }

    /** {@inheritDoc} */
    @Override
    protected void tearDown() throws Exception {
        super.tearDown();
        db.shutdown();
    }
}
