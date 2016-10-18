package org.ff4j.test.property;

/*
 * #%L
 * ff4j-core
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


import static org.ff4j.utils.JdbcUtils.isTableExist;

import javax.sql.DataSource;

import org.ff4j.property.store.JdbcPropertyStore;
import org.ff4j.store.JdbcQueryBuilder;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * Check DB and create Schema.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class JdbcPropertyStoreSchemaTest {

    /** DataBase. */
    private EmbeddedDatabase db;

    /** Builder. */
    private EmbeddedDatabaseBuilder builder = null;
    
    /** Tested Store. */
    protected JdbcPropertyStore testedStore;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        initStore();
    }
    
    /** {@inheritDoc} */
    public void initStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).build();
        testedStore = new JdbcPropertyStore();
        testedStore.setDataSource(db);
    }
   
    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }
    
    @Test
    public void testCreateSchema() {
        DataSource       ds = testedStore.getDataSource();
        JdbcQueryBuilder qb = testedStore.getQueryBuilder();
        // Given
        Assert.assertFalse(isTableExist(ds, qb.getTableNameProperties()));
        // When
        testedStore.createSchema();
        // then
        Assert.assertTrue(isTableExist(ds, qb.getTableNameProperties()));
        // When (no error)
        testedStore.createSchema();
    }
    
}
