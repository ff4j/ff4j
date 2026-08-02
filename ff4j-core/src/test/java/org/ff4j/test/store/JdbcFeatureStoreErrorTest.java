package org.ff4j.test.store;

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
import static org.junit.jupiter.api.Assertions.assertThrows;
import static org.mockito.Mockito.doThrow;

import java.sql.SQLException;
import java.sql.Statement;
import java.util.ArrayList;
import java.util.List;

import javax.sql.DataSource;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.store.JdbcFeatureStore;
import org.ff4j.store.JdbcStoreConstants;
import org.ff4j.utils.JdbcUtils;
import org.junit.jupiter.api.Test;
import org.mockito.Mockito;

public class JdbcFeatureStoreErrorTest {
    
    @Test
    public void testExecuteUpdate()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcUtils.executeUpdate(mockDS, "toto");
        });
    }
    
    @Test
    public void testgetiStableExist()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcUtils.isTableExist(mockDS, "toto");
        });
    }

    @Test
    public void testJdbcUtilCloseStatement()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            Statement statement = Mockito.mock(Statement.class);
            doThrow(new SQLException()).when(statement).close();
            JdbcUtils.closeStatement(null);
            JdbcUtils.rollback(null);
            JdbcUtils.closeStatement(statement);
        });
    }
    
    @Test
    public void testgetExistKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.exist("xx");
        });
    }
    
    @Test
    public void testgetReadKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            // Exist goes before
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.read("xx");
        });
    }
    
    @Test
    public void testgetReadGroupKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.readGroup("xx");
        });
    }
    
    @Test
    public void testCreateKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.create(new Feature("U1", true));
        });
    }
    
    @Test
    public void tesDeleteKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.delete("p1");
        });
    }
    
    @Test
    public void testreadAllKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.readAll();
        });
    }
    
    @Test
    public void testClearKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.clear();
        });
    }
    
    @Test
    public void testUpdateKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.update(new Feature("f1", true));
        });
    }
    
    @Test
    public void testReadAllGroupKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.readAllGroups();
        });
    }
    
    @Test
    public void testReadGroupKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.readGroup("invalid");
        });
    }
    
    @Test
    public void testexistGroupKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.existGroup("invalid");
        });
    }
    
    @Test
    public void testcreateCustomKO()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            List<Property<?>> lp = new ArrayList<Property<?>>();
            lp.add(new PropertyString("p1", "v1"));
            lp.add(new PropertyString("p2", "v2"));
            jrepo.createCustomProperties("F1", lp);
        });
    }
    
    @Test
    public void testJetDataSourceKO()  throws SQLException {
        assertThrows(IllegalStateException.class, () -> {
            JdbcFeatureStore jrepo = new JdbcFeatureStore(null);
            jrepo.getDataSource();
        });
    }
    
    @Test
    public void testUpdate2O()  throws SQLException {
        assertThrows(FeatureAccessException.class, () -> {
            DataSource mockDS = Mockito.mock(DataSource.class);
            doThrow(new SQLException()).when(mockDS).getConnection();
            JdbcFeatureStore jrepo = new JdbcFeatureStore(mockDS);
            jrepo.setDataSource(mockDS);
            jrepo.update(JdbcStoreConstants.SQL_DISABLE, "F4");
        });
    }


}
