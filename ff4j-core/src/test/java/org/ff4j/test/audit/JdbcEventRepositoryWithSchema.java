package org.ff4j.test.audit;

/*-
 * #%L
 * ff4j-core
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

import org.ff4j.audit.Event;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.audit.repository.JdbcEventRepository;
import org.ff4j.store.JdbcQueryBuilder;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

import java.sql.SQLException;

public class JdbcEventRepositoryWithSchema {

    private EmbeddedDatabase schemaNamedDb;
    /** Builder. */
    private EmbeddedDatabaseBuilder builder = new EmbeddedDatabaseBuilder();
    private JdbcEventRepository repo;

    @Before
    public void setUp() throws Exception {
        schemaNamedDb = builder.setType(EmbeddedDatabaseType.HSQL).//
                addScript("classpath:named-schema-ddl.sql").//
                addScript("classpath:schemanamed-ff-store.sql").//
                build();
    }

    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        Thread.sleep(200);
        schemaNamedDb.shutdown();
    }

    protected EventRepository initRepository() {
        repo = new JdbcEventRepository(schemaNamedDb);
        JdbcQueryBuilder queryBuilder = new JdbcQueryBuilder();
        queryBuilder.setDbSchema("FF4J");
        repo.setQueryBuilder(queryBuilder);

        return repo;
    }

    @Test
    public void testPeristance() throws SQLException {
        initRepository();

        repo.saveEvent(
            new Event("pSource", "pType", "pName", "pAction")
        );
    }
}
