package org.ff4j.elastic.store;

/*-
 * #%L
 * ff4j-store-elastic
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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

import static org.ff4j.elastic.ElasticQueryBuilder.createFeature;
import static org.ff4j.elastic.ElasticQueryBuilder.deleteFeature;
import static org.ff4j.elastic.ElasticQueryBuilder.findAllFeatures;
import static org.ff4j.elastic.ElasticQueryBuilder.findFeatureByUid;
import static org.ff4j.elastic.ElasticQueryBuilder.findFeaturesByGroupName;
import static org.ff4j.elastic.ElasticQueryBuilder.updateFeatureAddToGroup;
import static org.ff4j.elastic.ElasticQueryBuilder.updateFeatureRemoveFromGroup;
import static org.ff4j.elastic.ElasticQueryHelper.createIndexIfNotExist;
import static org.ff4j.elastic.ElasticQueryHelper.findFeatureTechIdFromUid;
import static org.ff4j.elastic.ElasticQueryHelper.findFeatureTechIdsFromGroupName;
import static org.ff4j.elastic.ElasticQueryHelper.toggle;

import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.searchbox.client.JestClient;
import io.searchbox.core.Index;
import io.searchbox.core.Search;
import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;

/**
 * Implementation of the {@link FeatureStore} to work ElasticSearch storage DB.
 *
 * @since 1.6
 * @since 1.8 Use only JestClient
 *
 * @author Cedrick Lunven (@clunven)
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 */
public class FeatureStoreElastic extends AbstractFeatureStore {
   
    /** Logger for the class. */
    private static final Logger LOGGER = LoggerFactory.getLogger(FeatureStoreElastic.class);
   
    /** if no value provide for index use this one. */
    public static String DEFAULT_INDEX_FEATURES = "ff4j_features";
    
    /** Injection of connection to elastic. */
    private JestClient jestClient;
    
    /** Default name of the index in elastic. */
    private String indexFeatures = DEFAULT_INDEX_FEATURES;

    /**
     * Default constructor.
     */
    public FeatureStoreElastic() {}

    /**
     * Initialization through {@link ElasticConnection}.
     *
     * @param connection
     *            current client to Elasticsearch database
     */
    public FeatureStoreElastic(JestClient jestClient) {
        this(jestClient, DEFAULT_INDEX_FEATURES);
    }

    /**
     * Initialization with Connection and initialisation file.
     *
     * @param connection
     * @param xmlFile
     */
    public FeatureStoreElastic(JestClient jestClient, String indexName) {
        this.jestClient    = jestClient;
        this.indexFeatures = indexName;
        createSchema();
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        LOGGER.info("Creating index {} (if needed)", indexFeatures);
        createIndexIfNotExist(jestClient, indexFeatures);
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        toggle(jestClient, indexFeatures, uid, true);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        toggle(jestClient, indexFeatures, uid, false);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        try {
            Search query  = findFeatureByUid(indexFeatures, uid);
            SearchResult result = jestClient.execute(query);
            if (!result.isSucceeded()) {
                throw new IllegalStateException(
                        "Error in query '" + result.getErrorMessage() + "'") ;
            }
            return (result != null) && 
                   (result.getHits(Map.class) != null) && 
                   !result.getHits(Map.class).isEmpty();
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot check feature existence for '" + uid + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        try {
            Index creationQuery = createFeature(indexFeatures, fp);
            jestClient.execute(creationQuery);
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot create feature '" + fp.getUid() + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        try {
            Search search = findFeatureByUid(indexFeatures, uid);
            return jestClient.execute(search).getFirstHit(Feature.class).source;
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot read feature '" + uid + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Map<String, Feature> mapOfFeatures = new HashMap<String, Feature>();
        try {
            Search query = findAllFeatures(indexFeatures);
            for (Hit<Feature, Void> feature : jestClient.execute(query).getHits(Feature.class)) {
                mapOfFeatures.put(feature.source.getUid(), feature.source);
            }
            return mapOfFeatures;
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot read  all features", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        try {
            String techid = findFeatureTechIdFromUid(jestClient, indexFeatures, uid);
            jestClient.execute(deleteFeature(indexFeatures, techid, uid));
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot delete feature '" + uid + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature feature) {
        assertFeatureNotNull(feature);
        assertFeatureExist(feature.getUid());
        try {
            // Replacing is faster than update
            String techid = findFeatureTechIdFromUid(jestClient, indexFeatures, feature.getUid());
            jestClient.execute(deleteFeature(indexFeatures, techid, feature.getUid()));
            jestClient.execute(createFeature(indexFeatures, feature));
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot update feature '" + feature.getUid() + "'", e);
        }   
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        assertFeatureExist(flipId);
        Util.assertHasLength(roleName);
        Feature feature = read(flipId);
        feature.getPermissions().add(roleName);
        update(feature);
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        assertFeatureExist(flipId);
        Util.assertHasLength(roleName);
        Feature feature = read(flipId);
        feature.getPermissions().remove(roleName);
        update(feature);
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        readGroup(groupName)
            .keySet().stream()
            .forEach(uid -> toggle(jestClient, indexFeatures, uid, true));
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        readGroup(groupName)
            .keySet().stream()
            .forEach(uid -> toggle(jestClient, indexFeatures, uid, false));
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        Set<String> featureIds = 
                findFeatureTechIdsFromGroupName(jestClient, indexFeatures, groupName);
        return (featureIds != null) && (featureIds.size() >= 1);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        try {
            Map<String, Feature> mapOfFeatures = new HashMap<String, Feature>();
            Search query = findFeaturesByGroupName(indexFeatures, groupName);
            for (Hit<Feature, Void> feature : jestClient.execute(query).getHits(Feature.class)) {
                mapOfFeatures.put(feature.source.getUid(), feature.source);
            }
            return mapOfFeatures;
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot read features from group '" + groupName + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        Util.assertHasLength(groupName);
        try {
            String techid = findFeatureTechIdFromUid(jestClient, indexFeatures, uid);
            jestClient.execute(updateFeatureAddToGroup(indexFeatures, techid, groupName));
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot add feature '" + uid + "' to group '" + groupName + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        try {
            String techid = findFeatureTechIdFromUid(jestClient, indexFeatures, uid);
            jestClient.execute(updateFeatureRemoveFromGroup(indexFeatures, techid));
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot remove feature '" + uid + "' to group '" + groupName + "'", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        Set<String> groups = new HashSet<String>();
        for (Map.Entry<String, Feature> entry : readAll().entrySet()) {
            if (null != entry.getValue().getGroup()) {
                groups.add(entry.getValue().getGroup());
            }
        }
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        try {
            jestClient.execute(ElasticQueryBuilder.deleteAllFeatures(indexFeatures));
        } catch (IOException e) {
            throw new FeatureAccessException("Cannot remove all features", e);
        }
    }

    /**
     * Getter accessor for attribute 'indexFeatures'.
     *
     * @return
     *       current value of 'indexFeatures'
     */
    public String getIndexFeatures() {
        return indexFeatures;
    }

    /**
     * Setter accessor for attribute 'indexFeatures'.
     * @param indexFeatures
     * 		new value for 'indexFeatures '
     */
    public void setIndexFeatures(String indexFeatures) {
        this.indexFeatures = indexFeatures;
    }
    
}
