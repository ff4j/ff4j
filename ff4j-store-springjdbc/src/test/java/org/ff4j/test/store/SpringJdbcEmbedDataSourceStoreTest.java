package org.ff4j.test.store;

/*
 * #%L
 * ff4j-store-springjdbc
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import org.ff4j.springjdbc.store.FeatureStoreSpringJdbc;
import org.junit.After;
import org.junit.Before;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class SpringJdbcEmbedDataSourceStoreTest extends FeatureStoreTestSupport {

    /** DataBase. */
    private EmbeddedDatabase db;

    /** Database builder. */
    private EmbeddedDatabaseBuilder builder = null;

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL)//
                .addScript("classpath:schema-ddl.sql")//
                .addScript("classpath:ff-store.sql")//
                .build();
        FeatureStoreSpringJdbc jdbcStore = new FeatureStoreSpringJdbc();
        jdbcStore.setDataSource(db);
        jdbcStore.getJdbcTemplate();
        return jdbcStore;
    }

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        db = builder.setType(EmbeddedDatabaseType.HSQL).//
                addScript("classpath:schema-ddl.sql").//
                addScript("classpath:ff-store.sql")
                .build();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }

}
