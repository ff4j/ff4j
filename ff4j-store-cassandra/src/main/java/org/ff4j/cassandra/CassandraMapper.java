package org.ff4j.cassandra;

import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_ACTION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_DURATION;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_HOSTNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_KEYS;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_NAME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_SOURCE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TIME;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_TYPE;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_UID;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_USER;
import static org.ff4j.cassandra.CassandraConstants.COL_EVENT_VALUE;

/*
 * #%L
 * ff4j-store-cassandra
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

import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_CUSTOMPROPERTIES;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_DESCRIPTION;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_ENABLE;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_GROUPNAME;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_ROLES;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_STRATEGY;
import static org.ff4j.cassandra.CassandraConstants.COL_FEAT_UID;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_CLAZZ;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_DESCRIPTION;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_FIXED;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_ID;
import static org.ff4j.cassandra.CassandraConstants.COL_PROPERTY_VALUE;

import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.audit.Event;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.FeatureJsonParser;
import org.ff4j.utils.json.PropertyJsonParser;

import com.datastax.driver.core.Row;

/**
 * Map result from cassandra query to target FF4J objects.
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class CassandraMapper {
    
    /**
     * Hide public constructor.
     */
    private CassandraMapper() {
    }
    
    /**
     * Marshall bean {@link Feature} from line in Cassandra table.
     *
     * @param row
     *      current line
     * @return
     *      Populated object
     */
    public static Feature mapFeature(Row row) {
        Feature f = new Feature(row.getString(COL_FEAT_UID));
        f.setDescription(row.getString(COL_FEAT_DESCRIPTION));
        f.setEnable(1 == row.getInt(COL_FEAT_ENABLE));
        f.setGroup(row.getString(COL_FEAT_GROUPNAME));
        f.setPermissions(mapFeaturePermissions(row));
        // Custom Properties
        Map < String, String > mapOfProperties = row.getMap(COL_FEAT_CUSTOMPROPERTIES,String.class, String.class);
        if (mapOfProperties != null) {
            Map < String, Property<?>> customProperties = new HashMap<String, Property<?>>();
            for (Map.Entry<String, String> propString : mapOfProperties.entrySet()) {
                customProperties.put(propString.getKey(), PropertyJsonParser.parseProperty(propString.getValue()));
            }
            f.setCustomProperties(customProperties);
        }
        // Flipping Strategy
        String jsonFlippingStrategy = row.getString(COL_FEAT_STRATEGY);
        if (Util.hasLength(jsonFlippingStrategy)) {
            f.setFlippingStrategy(FeatureJsonParser.parseFlipStrategyAsJson(f.getUid(), jsonFlippingStrategy));
        }
        return f;
    }
    
    /**
     * Extrat permissions from the row returned by cassandra.
     *  
     * @param row
     *      current cassandra row
     * @return
     *      target permission set
     */
    public static Set < String > mapFeaturePermissions(Row row) {
        return row.getSet(COL_FEAT_ROLES, String.class);
    }
    
    /**
     * Marshall from cassandra row to bean {@link Property}.
     *
     * @param row
     *      current cassandra row
     * @return
     *      target property bean
     */
    public static Property<?> mapProperty(Row row) {
        return PropertyFactory.createProperty(
                row.getString(COL_PROPERTY_ID), 
                row.getString(COL_PROPERTY_CLAZZ), 
                row.getString(COL_PROPERTY_VALUE),
                row.getString(COL_PROPERTY_DESCRIPTION), 
                row.getSet(COL_PROPERTY_FIXED, String.class));
    }
    
    /**
     * Marshall from cassandra row to bean {@link Event}.
     *
     * @param row
     *      current cassandra row
     * @return
     *      target property bean
     */
    public static Event mapEvent(Row row) {
        if (row == null) return null;
        Event evt = new Event(row.getString(COL_EVENT_SOURCE),
                row.getString(COL_EVENT_TYPE),
                row.getString(COL_EVENT_NAME),
                row.getString(COL_EVENT_ACTION));
        evt.setUuid(row.getString(COL_EVENT_UID));
        evt.setCustomKeys(row.getMap(COL_EVENT_KEYS, String.class, String.class));
        evt.setDuration(row.getLong(COL_EVENT_DURATION));
        evt.setHostName(row.getString(COL_EVENT_HOSTNAME));
        evt.setTimestamp(row.getTimestamp(COL_EVENT_TIME).getTime());
        evt.setUser(row.getString(COL_EVENT_USER));
        evt.setValue(row.getString(COL_EVENT_VALUE));
        return evt;
    }

}
