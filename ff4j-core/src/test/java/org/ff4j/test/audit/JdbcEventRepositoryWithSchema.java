package org.ff4j.test.audit;

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
