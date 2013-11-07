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

import static org.mockito.Matchers.any;
import static org.mockito.Matchers.anyVararg;
import static org.mockito.Mockito.when;

import java.sql.SQLException;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.test.AbstractStoreTest;
import org.junit.After;
import org.junit.Before;
import org.junit.Ignore;
import org.junit.Test;
import org.mockito.Mockito;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

public class JdbcFeatureStoreCoreTest extends AbstractStoreTest {

    /** DataBase. */
    private EmbeddedDatabase db;

    private EmbeddedDatabaseBuilder builder = null;

    @Override
    protected FeatureStore initStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).addScript("classpath:schema-ddl.sql").addScript("classpath:ff-store.sql")
                .build();

        JdbcFeatureStore jdbcStore = new JdbcFeatureStore();
        jdbcStore.setDataSource(db);
        return jdbcStore;
    }

    @Ignore
    @SuppressWarnings("unchecked")
    @Test(expected = FeatureAccessException.class)
    public void testErrorOnUpdate() throws SQLException {
        JdbcFeatureStore spy = (JdbcFeatureStore) Mockito.spy(testedStore);
        when(spy.buildStatement(any(String.class), (String[]) anyVararg())).thenThrow(SQLException.class);
        spy.update(new Feature("abc", false, "desca2"));
    }

    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        db = builder.setType(EmbeddedDatabaseType.HSQL).addScript("classpath:schema-ddl.sql").addScript("classpath:ff-store.sql")
                .build();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }
}
