package org.ff4j.elastic.store;

import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.elastic.ElasticConnection;
import org.ff4j.elastic.ElasticQueryBuilder;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;

import io.searchbox.core.SearchResult;
import io.searchbox.core.SearchResult.Hit;

/*
 * #%L ff4j-store-elastic %% Copyright (C) 2013 - 2016 FF4J %% Licensed under the Apache License, Version 2.0 (the "License"); you
 * may not use this file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

/**
 * Implementation of the {@link FeatureStore} to work ElasticSearch storage DB.
 *
 * @since 1.6
 *
 * @author C&eacute;drick Lunven (@clunven)
 * @author <a href="mailto:andre.blaszczyk@gmail.com">Andre Blaszczyk</a>
 */
public class FeatureStoreElastic extends AbstractFeatureStore {

    /** Injection of connection to elastic. */
    private ElasticConnection connection;

    /** Connection to store Elastic. */
    private ElasticQueryBuilder builder;

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
    public FeatureStoreElastic(ElasticConnection connection) {
        this.connection = connection;
    }

    /**
     * Initialization with Connection and initialisation file.
     *
     * @param connection
     * @param xmlFile
     */
    public FeatureStoreElastic(ElasticConnection connection, String xmlFile) {
        this(connection);
        importFeaturesFromXmlFile(xmlFile);
        getConnection().execute(getBuilder().queryFlushIndex());
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        assertFeatureExist(uid);
        getConnection().execute(getBuilder().queryEnable(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        assertFeatureExist(uid);
        getConnection().execute(getBuilder().queryDisable(uid));
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        SearchResult result = getConnection().search(getBuilder().queryGetFeatureById(uid), true);
        return (result.getTotal() != null) && (result.getTotal() >= 1);
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureNotExist(fp.getUid());
        getConnection().execute(getBuilder().queryCreateFeature(fp));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        assertFeatureExist(uid);
        // first hit is ensured as feature exist
        return getConnection().search(
                getBuilder().queryGetFeatureById(uid)).getFirstHit(Feature.class).source;
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        SearchResult search = getConnection().search(getBuilder().queryReadAllFeatures(), true);
        Map<String, Feature> mapOfFeatures = new HashMap<String, Feature>();
        if (null != search && search.isSucceeded()) {
            Integer total = search.getTotal();
            SearchResult searchAllResult = getConnection().search(getBuilder().queryReadAllFeatures(total), true);
            if (null != searchAllResult && searchAllResult.isSucceeded()) {
                for (Hit<Feature, Void> feature : searchAllResult.getHits(Feature.class)) {
                    mapOfFeatures.put(feature.source.getUid(), feature.source);
                }
            }
        }
        return mapOfFeatures;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        assertFeatureExist(uid);
        getConnection().execute(getBuilder().queryDeleteFeature(uid));
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        assertFeatureNotNull(fp);
        assertFeatureExist(fp.getUid());
        getConnection().execute(getBuilder().queryUpdateFeature(fp));
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        assertFeatureExist(flipId);
        Util.assertHasLength(roleName);

        Feature feature = read(flipId);
        feature.getPermissions().add(roleName);
        getConnection().execute(getBuilder().queryUpdateFeature(feature));
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        assertFeatureExist(flipId);
        Util.assertHasLength(roleName);

        Feature feature = read(flipId);
        feature.getPermissions().remove(roleName);
        getConnection().execute(getBuilder().queryUpdateFeature(feature));
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        assertGroupExist(groupName);
        for (String _id : getBuilder().getFeatureTechIdByGroup(groupName)) {
            getConnection().execute(getBuilder().queryEnableWithTechId(_id));
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        assertGroupExist(groupName);
        for (String _id : getBuilder().getFeatureTechIdByGroup(groupName)) {
            getConnection().execute(getBuilder().queryDisableWithTechId(_id));
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertParamHasLength(groupName, "groupName");
        SearchResult result = getConnection().search(getBuilder().getGroupByGroupName(groupName), true);
        return (result.getTotal() != null) && (result.getTotal() >= 1);
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        assertGroupExist(groupName);
        SearchResult result = getConnection().search(getBuilder().queryReadGroup(groupName));
        LinkedHashMap<String, Feature> mapOfFeatures = new LinkedHashMap<String, Feature>();
        if (null != result && result.isSucceeded()) {
            for (Hit<Feature, Void> hit : result.getHits(Feature.class)) {
                mapOfFeatures.put(hit.source.getUid(), hit.source);
            }
        }
        return mapOfFeatures;
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        Util.assertHasLength(groupName);
        getConnection().execute(getBuilder().queryAddFeatureToGroup(uid, groupName));
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        assertFeatureExist(uid);
        assertGroupExist(groupName);
        getConnection().execute(getBuilder().queryRemoveFeatureFromGroup(uid, groupName));
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
        getConnection().execute(getBuilder().queryClear());
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        getConnection().execute(getBuilder().queryFlushIndex());
    }

    /**
     * Getter accessor for attribute 'connection'.
     *
     * @return current value of 'connection'
     */
    public ElasticConnection getConnection() {
        return connection;
    }

    /**
     * Setter accessor for attribute 'connection'.
     *
     * @param connection
     *            new value for 'connection '
     */
    public void setConnection(ElasticConnection connection) {
        this.connection = connection;
    }

    /**
     * Getter accessor for attribute 'builder'.
     *
     * @return current value of 'builder'
     */
    public ElasticQueryBuilder getBuilder() {
        if (builder == null) {
            builder = new ElasticQueryBuilder(this.connection);
        }
        return builder;
    }
}
