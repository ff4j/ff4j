package org.ff4j.store;

/*
 * #%L
 * ff4j-store-springjdbc
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


import java.util.Map;
import java.util.Set;

import javax.sql.DataSource;

import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.property.store.PropertyStore;
import org.ff4j.utils.Util;
import org.springframework.beans.factory.annotation.Required;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

/**
 * Implementation of {@link PropertyStore} with SpringJDBC.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
@Repository
public class PropertyStoreSpringJdbc extends AbstractPropertyStore implements JdbcStoreConstants {

    /** SQL DataSource. */
    private DataSource dataSource;

    /** Access to storage. */
    private JdbcTemplate jdbcTemplate;

    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        return 1 == getJdbcTemplate().queryForObject(SQL_PROPERTY_EXIST, Integer.class, name);
    }

    @Override
    public <T> void createProperty(Property<T> value) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Property<?> readProperty(String name) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void updateProperty(String name, String newValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public <T> void updateProperty(Property<T> fixedValue) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public void deleteProperty(String name) {
        // TODO Auto-generated method stub
        
    }

    @Override
    public Map<String, Property<?>> readAllProperties() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public Set<String> listPropertyNames() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void clear() {
        // TODO Auto-generated method stub
        
    }
    
    /**
     * @param dataSource
     *            the dataSource to set
     */
    @Required
    public void setDataSource(DataSource dataSource) {
        this.dataSource = dataSource;
    }

    /**
     * Getter accessor for attribute 'jdbcTemplate'.
     * 
     * @return current value of 'jdbcTemplate'
     */
    public JdbcTemplate getJdbcTemplate() {
        if (jdbcTemplate == null) {
            if (dataSource == null) {
                throw new IllegalStateException("ff4j-jdbc: DatabaseStore has not been properly initialized, datasource is null");
            }
            this.jdbcTemplate = new JdbcTemplate(dataSource);
        }
        return jdbcTemplate;
    }
   

}
