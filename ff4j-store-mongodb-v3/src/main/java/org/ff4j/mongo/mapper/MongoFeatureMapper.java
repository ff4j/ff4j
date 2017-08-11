package org.ff4j.mongo.mapper;

/*
 * #%L
 * ff4j-store-mongodb-v3
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

import static org.ff4j.mongo.MongoDbConstants.FEATURE_CUSTOMPROPERTIES;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_DESCRIPTION;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_ENABLE;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_EXPRESSION;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_GROUPNAME;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_ROLES;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_STRATEGY;
import static org.ff4j.mongo.MongoDbConstants.FEATURE_UUID;

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.bson.Document;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.mapper.FeatureMapper;
import org.ff4j.property.Property;
import org.ff4j.utils.JsonUtils;
import org.ff4j.utils.MappingUtil;

import com.mongodb.DBObject;
import com.mongodb.util.JSON;

public class MongoFeatureMapper implements FeatureMapper<Document> {
    
    private static final MongoPropertyMapper PMAPPER = new MongoPropertyMapper();

    /** {@inheritDoc} */
    @Override
    public Feature fromStore(Document document) {
        String featUid = document.getString(FEATURE_UUID);
        boolean status = document.getBoolean(FEATURE_ENABLE);

        Feature f = new Feature(featUid, status);
        f.setDescription(document.getString(FEATURE_DESCRIPTION));
        f.setGroup(document.getString(FEATURE_GROUPNAME));
        f.setPermissions(mapAuthorization(document));
        f.setFlippingStrategy(mapStrategy(featUid, document));
        f.setCustomProperties(mapCustomProperties(document));
        return f;
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
        if (document.containsKey(FEATURE_ROLES)) {
            for (Object role : (Iterable) document.get(FEATURE_ROLES)) {
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
        String strategy = (String) document.get(FEATURE_STRATEGY);
        Map < String, String > initParams = 
                MappingUtil.toMap((String) document.get(FEATURE_EXPRESSION));
        if (strategy != null && !"".equals(strategy)) {
            return MappingUtil.instanceFlippingStrategy(featUid, strategy, initParams);
        }
        return document.get(FEATURE_STRATEGY, FlippingStrategy.class);
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
        if (dbObject.containsKey(FEATURE_CUSTOMPROPERTIES)) {
            String properties = (String) dbObject.get(FEATURE_CUSTOMPROPERTIES);
            Map < String, DBObject > values = (Map<String, DBObject>) JSON.parse(properties);
            for (Map.Entry<String,DBObject> entry : values.entrySet()) {
                mapOfCustomProperties.put(entry.getKey(), PMAPPER.fromStore(entry.getValue()));
            }
        }
        return mapOfCustomProperties;
    }
    
    @Override
    public Document toStore(Feature feature) {
        String strategyColumn = null;
        String expressionColumn = null;
        if (feature.getFlippingStrategy() != null) {
            strategyColumn = feature.getFlippingStrategy().getClass().getName();
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

    
    
}
