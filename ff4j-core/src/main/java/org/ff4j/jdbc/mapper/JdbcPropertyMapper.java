package org.ff4j.jdbc.mapper;

import java.sql.Connection;
import java.sql.PreparedStatement;

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

import org.ff4j.feature.exception.FeatureAccessException;
import org.ff4j.jdbc.JdbcConstants.PropertyColumns;
import org.ff4j.jdbc.JdbcQueryBuilder;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.PropertyFactory;
import org.ff4j.property.PropertyString;
import org.ff4j.property.exception.PropertyAccessException;
import org.ff4j.utils.Util;

/**
 * Convert resultset into {@link PropertyString}.
 *
 * @author Cedrick Lunven (@clunven)
 */
public class JdbcPropertyMapper extends AbstractJdbcMapper  implements PropertyMapper < PreparedStatement, ResultSet > {
    
    /**
     * Constructor with parameters.
     *
     * @param sqlConn
     *      connection sql
     * @param qbd
     *      query builder
     */
    public JdbcPropertyMapper(Connection sqlConn, JdbcQueryBuilder qbd) {
        super(sqlConn, qbd);
    }
    
    
    /** {@inheritDoc} */
    @Override
    public PreparedStatement mapToRepository(Property<?> property) {
        PreparedStatement ps;
        try {
            /** INSERT INTO FF4J_PROPERTY(UID,CREATED,
             *      LASTMODIFIED,OWNER,DESCRIPTION,CLASSNAME,
             *      READONLY,VAL,FIXEDVALUES)
             */
            ps = sqlConn.prepareStatement(queryBuilder.sqlInsertProperty());
            // 1...5
            populateEntity(ps, property);
            // ClassName
            ps.setString(6, property.getClassName());
            // Value
            ps.setString(7, property.asString());
            if (property.getFixedValues().isPresent()) {
                String fixedValues = property.getFixedValues().get().toString();
                ps.setString(8, fixedValues.substring(1, fixedValues.length() - 1));
            } else {
                ps.setString(8, null);
            }
        } catch (SQLException sqlEx) {
            throw new FeatureAccessException("Cannot create statement to create property", sqlEx);
        }
        return ps;
    }
    
    /** {@inheritDoc} */
    public Property<?> mapFeaturePropertyRepository(ResultSet rs) {
        try {
            String propertyUid   = rs.getString(PropertyColumns.UID.colname());
            String propertyClass = Property.mapPropertyType(rs.getString(PropertyColumns.CLASSNAME.colname()));
            String propertyValue = rs.getString(PropertyColumns.VALUE.colname());
            String fixedValues   = rs.getString(PropertyColumns.FIXEDVALUES.colname());
            Property<?> p = PropertyFactory.createProperty(propertyUid, propertyClass, propertyValue);
            populateFixedValues(fixedValues, p);
            return p;
        } catch (SQLException sqlEx) {
            throw new PropertyAccessException("Cannot map Resultset into property", sqlEx);
        }
    }
    
    private void populateFixedValues(String fixedValuesString, Property<?> currentProperty) {
        if (Util.hasLength(fixedValuesString)) {
            Arrays.stream(fixedValuesString.split(","))
                  .forEach(v-> currentProperty.add2FixedValueFromString(v.trim()) );
        }
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> mapFromRepository(ResultSet rs) {
        Property<?> p = mapFeaturePropertyRepository(rs);
        mapEntity(rs, p);
        return p;
    }
    
}
