package org.ff4j.test.store;

/*
 * #%L ff4j-core %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use
 * this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import org.ff4j.core.FeatureStore;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.store.JdbcQueryBuilder;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

public class JdbcFeatureStorePrefixTest extends CoreFeatureStoreTestSupport {

    /** DataBase. */
    private EmbeddedDatabase db;

    /** Builder. */
    private EmbeddedDatabaseBuilder builder = null;

    /** {@inheritDoc} */
    @Override
    protected FeatureStore initStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.
        		setType(EmbeddedDatabaseType.HSQL).//
        		addScript("classpath:ddl-prefix-schema.sql").//
        		addScript("classpath:ddl-prefix-data.sql").build();

        JdbcFeatureStore jdbcStore = new JdbcFeatureStore();
        jdbcStore.setQueryBuilder(new JdbcQueryBuilder("T_FF4J_", "_01"));
        jdbcStore.setDataSource(db);
        return jdbcStore;
    }

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        db = builder.setType(EmbeddedDatabaseType.HSQL).
        		addScript("classpath:ddl-prefix-schema.sql").//
        		addScript("classpath:ddl-prefix-data.sql").build();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testRemoveFromGroupInvalidGroup() {
        testedStore.removeFromGroup(F4, G0);
    }
}
