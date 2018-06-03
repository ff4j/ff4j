package org.ff4j.store.jdbc;

import org.ff4j.jdbc.JdbcQueryBuilder;
import org.junit.Test;

public class QueryBuilderTest {
    
    @Test
    public void testQuery() {
        JdbcQueryBuilder builder = new JdbcQueryBuilder();
        
        System.out.println(builder.sqlCreateSchema());
    }
    

}
