package org.ff4j.test.store;

import org.ff4j.springjdbc.store.FeatureStoreSpringJdbc;
import org.ff4j.springjdbc.store.PropertyStoreSpringJdbc;
import org.ff4j.store.JdbcQueryBuilder;
import org.ff4j.utils.JdbcUtils;
import org.junit.After;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabase;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseBuilder;
import org.springframework.jdbc.datasource.embedded.EmbeddedDatabaseType;

public class SpringJdbcStoresCreateSchema {
    
    /** DataBase. */
    private EmbeddedDatabase db;

    /** Builder. */
    private EmbeddedDatabaseBuilder builder = null;
    
    /** Tested Store. */
    protected FeatureStoreSpringJdbc testedStore;

    
    /** Tested Store. */
    protected PropertyStoreSpringJdbc propertyStore;

    /** {@inheritDoc} */
    @Before
    public void setUp() throws Exception {
        initStore();
    }
    
    /** {@inheritDoc} */
    public void initStore() {
        builder = new EmbeddedDatabaseBuilder();
        db = builder.setType(EmbeddedDatabaseType.HSQL).build();
        testedStore = new FeatureStoreSpringJdbc();
        testedStore.setDataSource(db);
        testedStore.getJdbcTemplate();
        
        propertyStore = new PropertyStoreSpringJdbc();
        propertyStore.setDataSource(db);
        propertyStore.getJdbcTemplate();
    }
   
    /** {@inheritDoc} */
    @After
    public void tearDown() throws Exception {
        db.shutdown();
    }
    
    @Test
    public void testCreateSchema() {
        JdbcQueryBuilder qb = testedStore.getQueryBuilder();
        // Given
        Assert.assertFalse(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameFeatures()));
        Assert.assertFalse(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameRoles()));
        Assert.assertFalse(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameCustomProperties()));
        // When
        testedStore.createSchema();
        propertyStore.createSchema();
        // then
        Assert.assertTrue(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameFeatures()));
        Assert.assertTrue(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameRoles()));
        Assert.assertTrue(JdbcUtils.isTableExist(
                testedStore.getJdbcTemplate().getDataSource(), qb.getTableNameCustomProperties()));
    }
    

}
