package org.ff4j.test.store;

import java.util.HashMap;

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


import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.utils.MappingUtil;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * This test is meant to access a Jfeature store in 'pure' JDBC.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcFeatureStoreTestInvalidData {

    /** DataBase. */
    private EmbeddedDatabase db;

    /** Target Store. */
    private JdbcFeatureStore jdbcStore;
   
    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        EmbeddedDatabaseBuilder builder = new EmbeddedDatabaseBuilder();
        db = builder.
                setType(EmbeddedDatabaseType.HSQL).//
                addScript("classpath:schema-ddl.sql").//
                addScript("classpath:ff-invalidstore.sql").build();

        jdbcStore = new JdbcFeatureStore();
        jdbcStore.setDataSource(db);
    }

    @Test(expected = FeatureAccessException.class)
    public void testReadInvalid() {
        jdbcStore.read("forth");
    }
    
    @Test(expected = FeatureAccessException.class)
    public void testInvalidStrategy() {
        MappingUtil.instanceFlippingStrategy("ID", "com.KO", new HashMap<String, String>());
    }
    
    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }

}
