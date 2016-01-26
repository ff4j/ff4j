package org.ff4j.test.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import org.ff4j.property.store.JdbcPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * Test for {@link JdbcPropertyStore}.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcPropertyStoreTest  extends AbstractPropertyStoreJunitTest {

    /** DataBase. */
    private EmbeddedDatabase db;

    /** Builder. */
    private EmbeddedDatabaseBuilder builder = null;

    /** {@inheritDoc} */
    @Override
    protected PropertyStore initPropertyStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).//
                addScript("classpath:schema-ddl.sql").//
                addScript("classpath:ff-store.sql").//
                build();
        return new JdbcPropertyStore(db);
    }
    
    /** {@inheritDoc} */
    @Override
    @Before
    public void setUp() throws Exception {
        super.setUp();
        db = builder.setType(EmbeddedDatabaseType.HSQL).//
                addScript("classpath:schema-ddl.sql").//
                addScript("classpath:ff-store.sql"). //
                
                build();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }
    
    @Test
    public void initJdbcPropertyStore() {
        EmbeddedDatabaseBuilder b2 = new EmbeddedDatabaseBuilder();
        EmbeddedDatabase db2 = b2.setType(EmbeddedDatabaseType.HSQL).//
                build();
        JdbcPropertyStore jdbcStore2 = new JdbcPropertyStore(db2, "ff4j.xml");
        Assert.assertNotNull(jdbcStore2);
    }
    
    @Test
    public void testClear() {
        EmbeddedDatabaseBuilder b2 = new EmbeddedDatabaseBuilder();
        EmbeddedDatabase db2 = b2.setType(EmbeddedDatabaseType.HSQL).//
                build();
        JdbcPropertyStore jdbcStore2 = new JdbcPropertyStore(db2, "ff4j.xml");
        Assert.assertNotNull(jdbcStore2);
    }
    
   
    
    
}
