package org.ff4j.property;

import javax.sql.DataSource;

/*-
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2018 FF4J
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

import org.ff4j.feature.repository.FeatureRepository;
import org.ff4j.jdbc.JdbcTestHelper;
import org.ff4j.property.repository.PropertyRepository;
import org.ff4j.property.repository.PropertyRepositoryJdbc;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;

/**
 * Testing implementation of {@link FeatureRepository} for DB : MEMORY
 *
 * @author Cedrick LUNVEN (@clunven)
 */
@DisplayName("Testing JDBC | PROPERTIES Repository")
public class RepositoryPropertiesStoreJdbcTest extends RepositoryPropertiesTestSupport {

    /** SQL DataSource. */
    private DataSource sqlDataSource;
    
    /** {@inheritDoc} */
    @Override
    public PropertyRepository initStore() {
        sqlDataSource = JdbcTestHelper.createInMemoryHQLDataSource();
        return new PropertyRepositoryJdbc(sqlDataSource);
    }
    
    /** {@inheritDoc} */
    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        JdbcTestHelper.initDBSchema(sqlDataSource);
    }
    
}
