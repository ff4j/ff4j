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


import org.ff4j.property.store.PropertyStore;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.After;
import org.junit.Before;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * Check {@link PropertyStore} implementation through standard super class.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class SpringJdbcPropertyStoreTest  extends PropertyStoreTestSupport {
    
    /** DataBase. */
    private EmbeddedDatabase db;

    /** Database builder. */
    private EmbeddedDatabaseBuilder builder = null;

    /** {@inheritDoc} */
    @Override
    protected  PropertyStore initPropertyStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL)//
                .addScript("classpath:schema-ddl.sql")//
                .addScript("classpath:ff-store.sql")//
                .build();
        PropertyStoreSpringJdbc jdbcStore = new PropertyStoreSpringJdbc();
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
