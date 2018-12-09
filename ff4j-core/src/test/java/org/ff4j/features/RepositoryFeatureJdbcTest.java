package org.ff4j.features;

import javax.sql.DataSource;

import org.ff4j.feature.repository.FeaturesRepository;
import org.ff4j.feature.repository.FeaturesRepositoryJdbc;
import org.ff4j.jdbc.JdbcTestHelper;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;

@DisplayName("Testing JDBC | FEATURES  Repository")
public class RepositoryFeatureJdbcTest extends RepositoryFeaturesTestSupport {
    
    /** SQL DataSource. */
    private DataSource sqlDataSource;
    
    /** Should reinit tables on each test , except first */
    private static boolean dropTable = false;
    
    /** {@inheritDoc} */
    @Override
    public FeaturesRepository initStore() {
        sqlDataSource = JdbcTestHelper.createInMemoryHQLDataSource();
        return new FeaturesRepositoryJdbc(sqlDataSource);
    }
    
    /** {@inheritDoc} */
    @Override
    @BeforeEach
    public void setUp() throws Exception {
        super.setUp();
        JdbcTestHelper.initDBSchema(sqlDataSource, dropTable);
        dropTable = true;
    }

}
