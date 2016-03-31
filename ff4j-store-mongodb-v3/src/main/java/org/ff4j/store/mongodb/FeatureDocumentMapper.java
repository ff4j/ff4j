package org.ff4j.store.mongodb;

import java.util.ArrayList;
import java.util.HashMap;

/*
 * #%L
 * ff4j-store-mongodb
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.bson.Document;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyJsonBean;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;

import com.mongodb.BasicDBList;
import com.mongodb.DBObject;
import com.mongodb.util.JSON;

import static org.ff4j.store.mongodb.FeatureStoreMongoConstants.*;

/**
 * MApping from Mongo document to Feature.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureDocumentMapper {

    /**
     * Convert {@link Document} to {@link Feature}.
     *
     * @param document
     *            document in mongodb.
     * @return
     */
    public Feature mapFeature(Document document) {
        String featUid = document.getString(UUID);
        boolean status = document.getBoolean(ENABLE);

        Feature f = new Feature(featUid, status);
        f.setDescription(document.getString(DESCRIPTION));
        f.setGroup(document.getString(GROUPNAME));
        f.setPermissions(mapAuthorization(document));
        f.setFlippingStrategy(mapStrategy(featUid, document));
        f.setCustomProperties(mapCustomProperties(document));
        return f;
    }
    
    /**
     * Map a property.
     *
     * @param dbObject
     *      db object
     * @return
     *      list of property
     */
    @SuppressWarnings("unchecked")
    public Property< ? > mapProperty(Document dbObject) {
        PropertyJsonBean pf = new PropertyJsonBean();
        pf.setName((String) dbObject.get(PROPERTY_NAME));
        pf.setDescription((String) dbObject.get(PROPERTY_DESCRIPTION));
        pf.setType((String) dbObject.get(PROPERTY_TYPE));
        pf.setValue((String) dbObject.get(PROPERTY_VALUE));
        if (dbObject.containsKey(PROPERTY_FIXEDVALUES)) {
            ArrayList<String> dbList = (ArrayList<String>) dbObject.get(PROPERTY_FIXEDVALUES);
            if (dbList != null) {
                for(Object item : dbList) {
                    pf.addFixedValue((String) item);
                }
            }
        }
        return pf.asProperty();
    }

    /**
     * Convert {@link Feature} to {@link Document}.
     *
     * @param feature
     *            target feature.
     * @return document in mongo db
     */
    public Document toDocument(Feature feature) {
        String strategyColumn = null;
        String expressionColumn = null;
        if (feature.getFlippingStrategy() != null) {
            strategyColumn = feature.getFlippingStrategy().getClass().getCanonicalName();
            expressionColumn = MappingUtil.fromMap(feature.getFlippingStrategy().getInitParams());
        }
        String customProperties = null;
        if (feature.getCustomProperties() != null) {
            customProperties = JsonUtils.customPropertiesAsJson(feature.getCustomProperties());
        }
        return new FeatureDocumentBuilder().addFeatUid(feature.getUid()).//
                addEnable(feature.isEnable()).//
                addDescription(feature.getDescription()).//
                addGroupName(feature.getGroup()).//
                addStrategy(strategyColumn).//
                addExpression(expressionColumn).//
                addCustomProperties(customProperties). //
                addRoles(feature.getPermissions()).build();
    }

    /**
     * Map from {@link Document} to authorizations..
     *
     * @param document
     *            target
     * @return
     */
    @SuppressWarnings("rawtypes")
    private Set<String> mapAuthorization(Document document) {
        Set<String> authorisation = new HashSet<String>();
        if (document.containsKey(ROLES)) {
            for (Object role : (Iterable) document.get(ROLES)) {
                authorisation.add(role.toString());
            }
        }
        return authorisation;
    }

    /**
     * Map from {@link DBObject} to strategy.
     * 
     * @param dbObject
     *            target
     * @return
     */
    private FlippingStrategy mapStrategy(String featUid, Document document) {
        String strategy = (String) document.get(STRATEGY);
        Map < String, String > initParams = MappingUtil.toMap((String) document.get(EXPRESSION));
        if (strategy != null && !"".equals(strategy)) {
            return MappingUtil.instanceFlippingStrategy(featUid, strategy, initParams);
        }
        return document.get(STRATEGY, FlippingStrategy.class);
    }
    
    /**
     * Custom Properties.
     *
     * @param dbObject
     *      db object
     * @return
     *      list of property
     */
    @SuppressWarnings("unchecked")
    private Map < String, Property<?> > mapCustomProperties(Document dbObject) {
        Map < String, Property<?> > mapOfCustomProperties = new HashMap<String, Property<?>>();
        if (dbObject.containsKey(CUSTOMPROPERTIES)) {
            String properties = (String) dbObject.get(CUSTOMPROPERTIES);
            Map < String, DBObject > values = (Map<String, DBObject>) JSON.parse(properties);
            for (Map.Entry<String,DBObject> entry : values.entrySet()) {
                mapOfCustomProperties.put(entry.getKey(), mapProperty(entry.getValue()));
            }
        }
        return mapOfCustomProperties;
    }
    
    public Document fromProperty2DBObject(Property<?> property) {
        PropertyJsonBean pjb = new PropertyJsonBean(property);
        return new PropertyDocumentBuilder().//
                addName(pjb.getName()). //
                addType(pjb.getType()). //
                addValue(pjb.getValue()). //
                addDescription(pjb.getDescription()). //
                addFixedValues(pjb.getFixedValues()).build();
    }
    
    /**
     * Map a property.
     *
     * @param dbObject
     *      db object
     * @return
     *      list of property
     */
    public Property< ? > mapProperty(DBObject dbObject) {
        PropertyJsonBean pf = new PropertyJsonBean();
        pf.setName((String) dbObject.get(PROPERTY_NAME));
        pf.setDescription((String) dbObject.get(PROPERTY_DESCRIPTION));
        pf.setType((String) dbObject.get(PROPERTY_TYPE));
        pf.setValue((String) dbObject.get(PROPERTY_VALUE));
        if (dbObject.containsField(PROPERTY_FIXEDVALUES)) {
            BasicDBList dbList = (BasicDBList) dbObject.get(PROPERTY_FIXEDVALUES);
            if (dbList != null) {
                for(Object item : dbList) {
                    pf.addFixedValue((String) item);
                }
            }
        }
        return pf.asProperty();
    }
}
