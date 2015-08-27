package org.ff4j.property.store;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.util.Arrays;
import java.util.List;

import org.ff4j.property.AbstractProperty;
import org.ff4j.property.Property;
import org.ff4j.store.JdbcStoreConstants;

/**
 * Convert resultset into {@link Property}.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class JdbcPropertyMapper implements JdbcStoreConstants {
    
    /**
     * Expect to convert a JDBC Result to Property.
     * @param rs
     *      current line resultset
     * @return
     *      target property
     * @throws SQLException
     */
    public AbstractProperty<?> map(ResultSet rs) throws SQLException {
        String propertyName  = rs.getString(COL_PROPERTY_ID);
        String propertyValue = rs.getString(COL_PROPERTY_VALUE);
        AbstractProperty<?> ap = new Property(propertyName, propertyValue);
       
        // Dedicated Type
        String propertyType = rs.getString(COL_PROPERTY_TYPE);
        if (propertyType != null) {
            try {
                // Construction by dedicated constructor with introspection
                Constructor<?> constr = Class.forName(propertyType).getConstructor(String.class, String.class);
                ap = (AbstractProperty<?>) constr.newInstance(propertyName, propertyValue);
            } catch (InstantiationException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check default constructor", e);
            } catch (IllegalAccessException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check visibility", e);
            } catch (ClassNotFoundException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' not found", e);
            } catch (InvocationTargetException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "'  error within constructor", e);
            } catch (NoSuchMethodException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' constructor not found", e);
            } catch (SecurityException e) {
                throw new IllegalArgumentException("Cannot instantiate '" + propertyType + "' check constructor visibility", e);
            }
        }
        
        // Is there any fixed Value ?
        String fixedValues = rs.getString(COL_PROPERTY_FIXED);
        if (fixedValues != null && !"".equals(fixedValues)) {
            List<String> listOfFixedValue = Arrays.asList(fixedValues.split(","));
            if (listOfFixedValue != null) {
                for (String v : listOfFixedValue) {
                    ap.add2FixedValueFromString(v.trim());
                }
                // Check fixed value
                if (ap.getFixedValues() != null && !ap.getFixedValues().contains(ap.getValue())) {
                    throw new IllegalArgumentException("Cannot create property <" + ap.getName() + "> invalid value <"
                            + ap.getValue() + "> expected one of " + ap.getFixedValues());
                }
            }
        }
        return ap;
    }
    
    
}
