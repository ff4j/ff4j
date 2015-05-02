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

import org.bson.Document;
import org.ff4j.core.Feature;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.utils.ParameterUtils;

/**
 * MApping from Mongo document to Feature.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public final class FeatureDocumentMapper implements FeatureStoreMongoConstants {

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
        return f;
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
            expressionColumn = ParameterUtils.fromMap(feature.getFlippingStrategy().getInitParams());
        }
        return new FeatureDocumentBuilder().addFeatUid(feature.getUid()).//
                addEnable(feature.isEnable()).//
                addDescription(feature.getDescription()).//
                addGroupName(feature.getGroup()).//
                addStrategy(strategyColumn).//
                addExpression(expressionColumn).//
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
     * Map from {@link Document} to strategy.
     *
     * @param document
     *            target
     * @return
     */
    private FlippingStrategy mapStrategy(String featUid, Document document) {
        String strategy =  document.getString(STRATEGY);
        if (strategy != null && !"".equals(strategy)) {
            try {
                FlippingStrategy flipStrategy = (FlippingStrategy) Class.forName(strategy).newInstance();
                flipStrategy.init(featUid, ParameterUtils.toMap( document.getString(EXPRESSION)));
                return flipStrategy;
            } catch (InstantiationException ie) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no default constructor available", ie);
            } catch (IllegalAccessException iae) {
                throw new FeatureAccessException("Cannot instantiate Strategy, no visible constructor", iae);
            } catch (ClassNotFoundException e) {
                throw new FeatureAccessException("Cannot instantiate Strategy, classNotFound", e);
            }
        }
        return document.get(STRATEGY, FlippingStrategy.class);
    }

}
