package org.ff4j.test.audit;

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

import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.JdbcEventRepository;
import org.junit.After;
import org.junit.Before;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

/**
 * Unit testing of JDBC implementation of {@link EventRepository}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcEventRepositoryTest extends AbstractEventRepositoryTest {
    
    /** DataBase. */
    private EmbeddedDatabase db;

    /** Builder. */
    private EmbeddedDatabaseBuilder builder = null;
    
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
        Thread.sleep(200);
        db.shutdown();
    }
    
    /** {@inheritDoc} */
    @Override
    protected EventRepository initRepository() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).//
                 addScript("classpath:schema-ddl.sql").//
                 addScript("classpath:ff-store.sql").//
                 build();
        return new JdbcEventRepository(db);
    }

}
