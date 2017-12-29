package org.ff4j.v1.property.store;

import static org.ff4j.v1.store.JdbcStoreConstants.*;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

import org.ff4j.v1.property.Property;
import org.ff4j.v1.property.PropertyString;
import org.ff4j.v1.property.util.PropertyFactory;

/**
 * Convert resultset into {@link PropertyString}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcPropertyMapper {
    
    /**
     * Expect to convert a JDBC Result to Property.
     * @param rs
     *      current line resultset
     * @return
     *      target property
     * @throws SQLException
     */
    public Property<?> map(ResultSet rs) throws SQLException {
        String propertyName  = rs.getString(COL_PROPERTY_ID);
        String propertyValue = rs.getString(COL_PROPERTY_VALUE);
        String propertyType  = rs.getString(COL_PROPERTY_TYPE);
        String description   = rs.getString(COL_PROPERTY_DESCRIPTION);
        String fixedValues   = rs.getString(COL_PROPERTY_FIXED);
        Set < String > value = null;
        if (fixedValues != null) {
            value = new HashSet<String>(Arrays.asList(fixedValues.split(",")));
        }
        return PropertyFactory.createProperty(propertyName, propertyType, propertyValue, description, value);
    }
    
}
