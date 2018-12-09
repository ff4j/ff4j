package org.ff4j.jdbc;

import org.ff4j.jdbc.JdbcQueryBuilder;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

public class JdbcSchemaTest {

    @Test
    @DisplayName("Generate SQL queries to creates table structures")
    public void testGenerateSchemaDDL() {
        System.out.println(new JdbcQueryBuilder().sqlCreateSchema());
    }
    
    @Test
    @DisplayName("Generate SQL queries to drop table structures")
    public void testDropchemaDDL() {
        System.out.println(new JdbcQueryBuilder().sqlDropSchema());
    }
    
    @Test
    @DisplayName("Show statement")
    public void showStatement() {
        //System.out.println(new JdbcQueryBuilder().sqlInsertFeatureProperty());
        //System.out.println(new JdbcQueryBuilder().sqlInsertFeaturePermission());
        System.out.println(new JdbcQueryBuilder().sqlInsertToggleStrategy());
        System.out.println(new JdbcQueryBuilder().sqlInsertToggleStrategyProperties());
    }
}
