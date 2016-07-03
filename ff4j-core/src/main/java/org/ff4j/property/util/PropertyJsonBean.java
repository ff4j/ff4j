package org.ff4j.property.util;

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


import java.io.Serializable;
import java.util.HashSet;

import org.ff4j.property.BasePropertyBean;
import org.ff4j.property.Property;

/**
 * JSON Expression.
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyJsonBean extends BasePropertyBean implements Serializable {
    
    /** Serial number.*/
    private static final long serialVersionUID = 7249710480883717637L;

    /**
     * Default constructor for instrospection.
     */
    public PropertyJsonBean() {
    }
    
    /**
     * Initialization from Property.
     *
     * @param property
     *      target property
     */
    public PropertyJsonBean(Property<?> property) {
        if (property != null) {
            this.name        = property.getName();
            this.description = property.getDescription();
            this.type        = property.getType();
            this.value       = property.asString();
            if (property.getFixedValues() != null) {
                fixedValues = new HashSet<String>();
                for (Object fv : property.getFixedValues()) {
                    fixedValues.add(fv.toString());
                }
            }
        }
    }
    
    /**
     * Work with properties.
     *
     * @param value
     *      add a fixed value to the set
     * @return
     */
    public PropertyJsonBean addFixedValue(String value) {
        if (fixedValues == null) {
            fixedValues = new HashSet<String>();
        }
        fixedValues.add(value);
        return this;
    }
    
    public Property< ? > asProperty() {
        return PropertyFactory.createProperty(this);
    }
    
    /** {@inheritDoc} */
    @Override
    public String toString() {
        return asJson();
    }
    
    /**
     * Format the property as a Json Expression.
     * 
     * @return
     *      target Json
     */
    public String asJson() {
        StringBuilder jsonExpression = new StringBuilder("{");
        jsonExpression.append("\"name\":\"" + name + "\"");
        jsonExpression.append(",\"description\":");
        jsonExpression.append((null == description) ? "null" : "\"" + description + "\"");
        jsonExpression.append(",\"type\":\"" + type + "\"");
        jsonExpression.append(",\"value\":");
        jsonExpression.append((null == value) ? "null" : "\"" + value + "\"");
        if (fixedValues ==null) {
            jsonExpression.append(",\"fixedValues\":null");
        } else {
            jsonExpression.append(",\"fixedValues\":[");
            boolean first = true;
            for (String auth : fixedValues) {
                jsonExpression.append(first ? "" : ",");
                jsonExpression.append("\"" + auth + "\"");
                first = false;
            }
            jsonExpression.append("]");
        }
        jsonExpression.append("}");
        return jsonExpression.toString();
    }
}
