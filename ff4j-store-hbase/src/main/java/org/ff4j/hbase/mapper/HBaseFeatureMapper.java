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


import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_CORE;
import static org.ff4j.hbase.HBaseConstants.B_FEATURES_CF_PROPERTIES;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_DESCRIPTION;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_ENABLE;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_GROUPNAME;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_ROLES;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_STRATEGY;
import static org.ff4j.hbase.HBaseConstants.B_FEAT_UID;

import java.util.Map;
import java.util.NavigableMap;

import org.apache.commons.lang.NotImplementedException;
import org.apache.hadoop.hbase.client.Put;
import org.apache.hadoop.hbase.client.Result;
import org.apache.hadoop.hbase.util.Bytes;
import org.ff4j.core.Feature;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

/**
 * Convert 
 * @author Cedrick LUNVEN (@clunven)
 */
public class HBaseFeatureMapper implements FeatureMapper<Put> {
    
    /** {@inheritDoc} */
    @Override
    public Put toStore(Feature fp) {
        Put put = new Put(Bytes.toBytes(fp.getUid()));
        // uid
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_UID, Bytes.toBytes(fp.getUid()));
        // description
        byte[] desc = (fp.getDescription() == null) ? null : Bytes.toBytes(fp.getDescription());
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_DESCRIPTION, desc);
        // enable
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_ENABLE, Bytes.toBytes(fp.isEnable()));
        // group
        byte[] group = (fp.getGroup() == null) ? null : Bytes.toBytes(fp.getGroup());
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_GROUPNAME, group);
        // permission
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_ROLES, 
                Bytes.toBytes(JsonUtils.permissionsAsJson(fp.getPermissions())));
        // Flipping strategy
        put.addColumn(B_FEATURES_CF_CORE, B_FEAT_STRATEGY, 
                Bytes.toBytes(JsonUtils.flippingStrategyAsJson(fp.getFlippingStrategy())));
        // Custom Properties
        if (fp.getCustomProperties() != null && !fp.getCustomProperties().isEmpty()) {
            for (Map.Entry<String, Property<?>> customP : fp.getCustomProperties().entrySet()) {
                if (customP.getValue() != null) {
                    put.addColumn(B_FEATURES_CF_PROPERTIES, 
                            Bytes.toBytes(customP.getKey()), Bytes.toBytes(customP.getValue().toJson()));
                }
            }
        }
        return put;
    }

    /** {@inheritDoc} */
    @Override
    public Feature fromStore(Put bean) {
        throw new NotImplementedException("Data retrieved from HBASE are GET (not PUT)");
    }
    
    /** {@inheritDoc} */
    public Feature fromStore(Result result) {
       // uid
       String uid = Bytes.toString(result.getValue(B_FEATURES_CF_CORE, B_FEAT_UID));
       Feature fout = new Feature(uid);
       // description
       fout.setDescription(
               Bytes.toString(result.getValue(B_FEATURES_CF_CORE, B_FEAT_DESCRIPTION)));
       // enable
       fout.setEnable(
               Bytes.toBoolean(result.getValue(B_FEATURES_CF_CORE, B_FEAT_ENABLE)));
       // group
       fout.setGroup(
               Bytes.toString(result.getValue(B_FEATURES_CF_CORE, B_FEAT_GROUPNAME)));
       if ("null".equals(fout.getGroup())) {
           fout.setGroup(null);
       }
       // permissions
       fout.setPermissions(FeatureJsonParser.parsePermissions(
               Bytes.toString(result.getValue(B_FEATURES_CF_CORE, B_FEAT_ROLES))));
       // Flipping Strategy
       fout.setFlippingStrategy(FeatureJsonParser.parseFlipStrategyAsJson(uid,
               Bytes.toString(result.getValue(B_FEATURES_CF_CORE, B_FEAT_STRATEGY))));
       // Custom Properties
       NavigableMap < byte[], byte[] > map = result.getFamilyMap(B_FEATURES_CF_PROPERTIES);
       for (Map.Entry<byte[], byte[]> property : map.entrySet()) {
           fout.getCustomProperties().put(
               Bytes.toString(property.getKey()), 
               PropertyJsonParser.parseProperty(
                       Bytes.toString(property.getValue())));
       }
       return fout;
    }

}
