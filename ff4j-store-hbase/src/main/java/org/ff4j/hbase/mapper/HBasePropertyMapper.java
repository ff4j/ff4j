package org.ff4j.hbase.mapper;

/*
 * #%L
 * ff4j-store-hbase
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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


import static org.ff4j.hbase.HBaseConstants.B_COL_PROPERTY_CLAZZ;
import static org.ff4j.hbase.HBaseConstants.B_COL_PROPERTY_DESCRIPTION;
import static org.ff4j.hbase.HBaseConstants.B_COL_PROPERTY_FIXED;
import static org.ff4j.hbase.HBaseConstants.B_COL_PROPERTY_ID;
import static org.ff4j.hbase.HBaseConstants.B_COL_PROPERTY_VALUE;
import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_PROPERTIES;

import java.util.HashSet;
import java.util.Set;

import org.apache.commons.lang.NotImplementedException;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.mapper.PropertyMapper;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.json.FeatureJsonParser;

/**
 * Convert 
 * @author Cedrick LUNVEN (@clunven)
 */
public class HBasePropertyMapper implements PropertyMapper<Put> {
    
    /** {@inheritDoc} */
    @Override
    public Put toStore(Property<?> prop) {
        Put put = new Put(Bytes.toBytes(prop.getName()));
        // uid
        byte[] propName = Bytes.toBytes(prop.getName());
        put.addColumn(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_ID, propName);
        // type
        byte[] mytype = (prop.getType() == null) ? null : Bytes.toBytes(prop.getType());
        put.addColumn(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_CLAZZ, mytype);
        // value
        byte[] pValue = (prop.asString() == null) ? null : Bytes.toBytes(prop.asString());
        put.addColumn(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_VALUE, pValue);
        // description
        byte[] desc = (prop.getDescription() == null) ? null : Bytes.toBytes(prop.getDescription());
        put.addColumn(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_DESCRIPTION, desc);
        // fixedValue
        Set < String > fixedValueAsString = new HashSet<>();
        if (prop.getFixedValues() != null) {
            for(Object o : prop.getFixedValues()) {
                fixedValueAsString.add(o.toString());
            }
            byte[] pFixedValue = Bytes.toBytes(JsonUtils.permissionsAsJson(fixedValueAsString));
            put.addColumn(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_FIXED, pFixedValue);
        }
        return put;
    }

    /** {@inheritDoc} */
    @Override
    public Property<?> fromStore(Put bean) {
        throw new NotImplementedException("Data retrieved from HBASE are GET (not PUT)");
    }
    
    /** {@inheritDoc} */
    public Property<?> fromStore(Result result) {
       String propName = Bytes.toString(result.getValue(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_ID));
       String propType = Bytes.toString(result.getValue(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_CLAZZ));
       String propVal  = Bytes.toString(result.getValue(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_VALUE));
       Property<?> prop = PropertyFactory.createProperty(propName, propType, propVal);
       String desc = Bytes.toString(result.getValue(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_DESCRIPTION));
       prop.setDescription(desc);
       Set < String > fixedValues = FeatureJsonParser.parsePermissions(
               Bytes.toString(result.getValue(B_FEATURES_CF_PROPERTIES, B_COL_PROPERTY_FIXED)));
       if (fixedValues != null) {
           fixedValues.forEach(prop::add2FixedValueFromString);
       }
       return prop;
    }

}
