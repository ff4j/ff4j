package org.ff4j.store.mongodb;

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
import java.util.Set;

import org.bson.types.BasicBSONList;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.utils.ParameterUtils;

import com.mongodb.DBObject;

/**
 * MApping from Mongo document to Feature.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureDBObjectMapper implements FeatureStoreMongoConstants {

    /**
     * Convert {@link DBObject} to {@link Feature}.
     * 
     * @param dbObject
     *            document in mongodb.
     * @return
     */
    public Feature mapFeature(DBObject dbObject) {
        String featUid = (String) dbObject.get(UUID);
        boolean status = (Boolean) dbObject.get(ENABLE);

        Feature f = new Feature(featUid, status);
        f.setDescription((String) dbObject.get(DESCRIPTION));
        f.setGroup((String) dbObject.get(GROUPNAME));
        f.setAuthorizations(mapAuthorization(dbObject));
        f.setFlippingStrategy(mapStrategy(featUid, dbObject));
        return f;
    }

    /**
     * Convert {@link Feature} to {@link DBObject}.
     * 
     * @param feature
     *            target feature.
     * @return document in mongo db
     */
    public DBObject toDBObject(Feature feature) {
        String strategyColumn = null;
        String expressionColumn = null;
        if (feature.getFlippingStrategy() != null) {
            strategyColumn = feature.getFlippingStrategy().getClass().getCanonicalName();
            expressionColumn = ParameterUtils.fromMap(feature.getFlippingStrategy().getInitParams());
        }
        return new FeatureDBObjectBuilder().addFeatUid(feature.getUid()).//
                addEnable(feature.isEnable()).//
                addDescription(feature.getDescription()).//
                addGroupName(feature.getGroup()).//
                addStrategy(strategyColumn).//
                addExpression(expressionColumn).//
                addRoles(feature.getAuthorizations()).build();
    }

    /**
     * Map from {@link DBObject} to authorizations..
     * 
     * @param dbObject
     *            target
     * @return
     */
    private Set<String> mapAuthorization(DBObject dbObject) {
        Set<String> authorisation = new HashSet<String>();
        if (dbObject.containsField(ROLES)) {
            for (Object role : (BasicBSONList) dbObject.get(ROLES)) {
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
    private FlippingStrategy mapStrategy(String featUid, DBObject dbObject) {
        String strategy = (String) dbObject.get(STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            try {
                FlippingStrategy flipStrategy = (FlippingStrategy) Class.forName(strategy).newInstance();
                flipStrategy.init(featUid, ParameterUtils.toMap((String) dbObject.get(EXPRESSION)));
                return flipStrategy;
            } catch (InstantiationException ie) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no default constructor available", ie);
            } catch (IllegalAccessException iae) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no visible constructor", iae);
            } catch (ClassNotFoundException e) {
                throw new FeatureAccessException("Cannot instantiate Strategy, classNotFound", e);
            }
        }
        return (FlippingStrategy) dbObject.get(STRATEGY);
    }

}
