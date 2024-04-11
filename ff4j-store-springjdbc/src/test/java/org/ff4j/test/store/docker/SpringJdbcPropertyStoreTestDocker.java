package org.ff4j.test.store.docker;

/*-
 * #%L
 * ff4j-store-springjdbc
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


import javax.sql.DataSource;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.ff4j.test.propertystore.PropertyStoreTestSupport;
import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.springframework.jdbc.datasource.DriverManagerDataSource;
import org.testcontainers.containers.PostgreSQLContainer;

/**
 * Check {@link PropertyStore} implementation through standard super class.
 *
 * @author Pedro Garcia Mota (@pedrompg)</a>
 */
public class SpringJdbcPropertyStoreTestDocker extends PropertyStoreTestSupport {

    /**
     * Constants.
     **/
    private static final String DOCKER_POSTGRES_IMAGE = "postgres:12-alpine";
    /**
     * Cassandra Containers.
     **/
    protected static PostgreSQLContainer<?> postgresContainer = null;
    /**
     * Datasource.
     */
    private DataSource dataSource;

    @BeforeClass
    public static void startDocker() {
        postgresContainer = new PostgreSQLContainer<>(DOCKER_POSTGRES_IMAGE);
        postgresContainer.start();
    }

    @AfterClass
    public static void stopDocker() {
        postgresContainer.stop();
    }

    public DataSource getDataSource() {
        DriverManagerDataSource dataSource = new DriverManagerDataSource();
        dataSource.setDriverClassName("org.postgresql.Driver");
        dataSource.setUrl(postgresContainer.getJdbcUrl());
        dataSource.setUsername(postgresContainer.getUsername());
        dataSource.setPassword(postgresContainer.getPassword());

        return dataSource;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    protected PropertyStore initPropertyStore() {
        if (dataSource == null) {
            dataSource = getDataSource();
        }
        PropertyStoreSpringJdbc jdbcStore = new PropertyStoreSpringJdbc();
        jdbcStore.setDataSource(dataSource);
        jdbcStore.getJdbcTemplate();
        jdbcStore.createSchema();
        jdbcStore.importPropertiesFromXmlFile("test-ff4j-features.xml");
        return jdbcStore;
    }
}
