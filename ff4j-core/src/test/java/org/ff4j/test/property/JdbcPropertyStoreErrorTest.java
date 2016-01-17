package org.ff4j.test.property;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2016 FF4J
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
import static org.mockito.Mockito.doThrow;

import java.sql.SQLException;

import javax.sql.DataSource;

import org.ff4j.exception.PropertyAccessException;
import org.ff4j.property.PropertyString;
import org.ff4j.property.store.JdbcPropertyStore;
import org.junit.Test;
import org.mockito.Mockito;

public class JdbcPropertyStoreErrorTest {
    
    @Test(expected = PropertyAccessException.class)
    public void testgetExistKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.existProperty("xx");
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testgetReadAll()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.readAllProperties();
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testCreateKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.createProperty(new PropertyString("p1","v1"));
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testReadKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.readProperty("p1");
    }
    
    @Test(expected = IllegalArgumentException.class)
    public void testUpdateKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.updateProperty(null);
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testUpdateKO2()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.updateProperty("p1", "v1");
    }
    
    @Test(expected = PropertyAccessException.class)
    public void tesDeleteKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.deleteProperty("p1");
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testListProperties()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.listPropertyNames();
    }
    
    @Test(expected = PropertyAccessException.class)
    public void testClearKO()  throws SQLException {
        DataSource mockDS = Mockito.mock(DataSource.class);
        doThrow(new SQLException()).when(mockDS).getConnection();
        JdbcPropertyStore jrepo = new JdbcPropertyStore(mockDS);
        jrepo.setDataSource(mockDS);
        jrepo.clear();
    }

}
