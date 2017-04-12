package org.ff4j.test.store;

import org.ff4j.springjdbc.store.FeatureStoreSpringJdbc;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.junit.Test;

public class TestSpringJdbcErrors {
    
    @Test(expected = IllegalStateException.class)
    public void testErrorOnDataSource() {
        FeatureStoreSpringJdbc s = new FeatureStoreSpringJdbc();
        s.getJdbcTemplate();
    }
    
    @Test(expected = IllegalStateException.class)
    public void testErrorOnDataSourceProperty() {
        PropertyStoreSpringJdbc s = new PropertyStoreSpringJdbc();
        s.getJdbcTemplate();
    }

}
