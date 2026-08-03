package org.ff4j.web.client.store;

/*-
 * #%L
 * ff4j-webapi-client
 * %%
 * Copyright (C) 2013 - 2026 FF4J
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

import static org.ff4j.utils.json.FeatureJsonParser.parseFeature;
import static org.ff4j.utils.json.FeatureJsonParser.parseFeatureArray;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ADDGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_DISABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ENABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_GRANTROLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEROLE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_STORE;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.ff4j.web.FF4jWebConstants.STORE_CREATESCHEMA;

import java.net.http.HttpClient;
import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.web.client.utils.ClientHttpUtils;
import org.ff4j.web.client.utils.HttpConnection;

import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.ObjectMapper;

/**
 * Implementation of {@link org.ff4j.core.FeatureStore} invoking the FF4j WebAPI
 * through the JDK {@link HttpClient} (no JAX-RS / Jersey dependency).
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class FeatureStoreHttp extends AbstractFeatureStore {

    /** String constants */
    private static final String OCCURED = " occured.";

    /** constant. */
    private static final String CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR = "Cannot grant role on feature, an HTTP error ";

    /** Http status. */
    private static final int OK = 200;

    /** Http status. */
    private static final int CREATED = 201;

    /** Http status. */
    private static final int NO_CONTENT = 204;

    /** Http status. */
    private static final int BAD_REQUEST = 400;

    /** Http status. */
    private static final int NOT_FOUND = 404;

    /** Jackson mapper to parse the group listing. */
    private static final ObjectMapper OBJECT_MAPPER = new ObjectMapper();

    /** Property to get url ROOT. */
    private String url = null;

    /** header parameter to add if secured mode enabled. */
    private String authorizationHeaderValue = null;

    /** Http connection to target API. */
    private HttpConnection connection = null;

    /**
     * Default construtor
     */
    public FeatureStoreHttp() {}

    /**
     * Initialization from URL.
     *
     * @param rootApiUrl target root URL
     */
    public FeatureStoreHttp(String rootApiUrl) {
        this.url = rootApiUrl;
    }

    /**
     * Authentication through APIKEY.
     *
     * @param rootApiUrl target url
     * @param apiKey target api
     */
    public FeatureStoreHttp(String rootApiUrl, String apiKey) {
        this(rootApiUrl);
        this.authorizationHeaderValue = ClientHttpUtils.buildAuthorization4ApiKey(apiKey);
    }

    /**
     * Authentication through login/password.
     *
     * @param rootApiUrl target url
     * @param username target username
     * @param password target password
     */
    public FeatureStoreHttp(String rootApiUrl, String username, String password) {
        this(rootApiUrl);
        this.authorizationHeaderValue = ClientHttpUtils.buildAuthorization4UserName(username, password);
    }

    /**
     * Lazy initialization of the http connection.
     *
     * @return target connection
     */
    public HttpConnection getConnection() {
        if (connection == null) {
            Util.assertNotNull(url);
            connection = new HttpConnection(url, authorizationHeaderValue);
        }
        return connection;
    }

    /**
     * Inject a pre-configured {@link HttpClient} (proxy, ssl context, timeouts...).
     *
     * @param httpClient target http client
     */
    public void setHttpClient(HttpClient httpClient) {
        getConnection().setHttpClient(httpClient);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        Util.assertHasLength(uid);
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_FEATURES, uid);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        } else if (OK != res.statusCode()) {
            throw new FeatureAccessException("Error when reaching API code:[" + res.statusCode() + "] MSG:" + res.body());
        }
        return parseFeature(res.body());
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Util.assertHasLength(uid);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_ENABLE);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        Util.assertHasLength(uid);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_DISABLE);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_FEATURES, uid);
        if (OK == res.statusCode()) {
            return true;
        }
        if (NOT_FOUND == res.statusCode()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of feature, an HTTP error " + res.statusCode() + " occured : " + res.body());
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (exist(fp.getUid())) {
            throw new FeatureAlreadyExistException(fp.getUid());
        }
        // Upsert through PUT HTTP method
        HttpResponse<String> res = getConnection().put(fp.toJson(), RESOURCE_STORE, RESOURCE_FEATURES, fp.getUid());
        // Check response code CREATED or raised error
        if (CREATED != res.statusCode()) {
            throw new FeatureAccessException("Cannot create feature, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_FEATURES);
        if (OK != res.statusCode()) {
            throw new FeatureAccessException("Cannot read features, an HTTP error " + res.statusCode() + OCCURED);
        }
        Feature[] fArray = parseFeatureArray(res.body());
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (Feature feature : fArray) {
            features.put(feature.getUid(), feature);
        }
        return features;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        Util.assertHasLength(uid);
        HttpResponse<String> res = getConnection().delete(RESOURCE_STORE, RESOURCE_FEATURES, uid);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException("Cannot delete feature, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        HttpResponse<String> res = getConnection().put(fp.toJson(), RESOURCE_STORE, RESOURCE_FEATURES, fp.getUid());
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException("Cannot update feature, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(uid, roleName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_GRANTROLE, roleName);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        Util.assertHasLength(uid, roleName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_REMOVEROLE, roleName);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException("Cannot remove role on feature, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        Util.assertHasLength(uid, groupName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_ADDGROUP, groupName);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException("Cannot add feature to group, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        Util.assertHasLength(uid, groupName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_FEATURES, uid, OPERATION_REMOVEGROUP, groupName);
        if (NOT_FOUND == res.statusCode()) {
            throw new FeatureNotFoundException(uid);
        }
        if (BAD_REQUEST == res.statusCode()) {
            throw new GroupNotFoundException(groupName);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException("Cannot remove feature from group, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        Util.assertHasLength(groupName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_GROUPS, groupName, OPERATION_ENABLE);
        if (NOT_FOUND == res.statusCode()) {
            throw new GroupNotFoundException(groupName);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        Util.assertHasLength(groupName);
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, RESOURCE_GROUPS, groupName, OPERATION_DISABLE);
        if (NOT_FOUND == res.statusCode()) {
            throw new GroupNotFoundException(groupName);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertHasLength(groupName);
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_GROUPS, groupName);
        if (NOT_FOUND == res.statusCode()) {
            throw new GroupNotFoundException(groupName);
        }
        if (OK != res.statusCode()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + res.statusCode() + OCCURED);
        }
        Feature[] fArray = parseFeatureArray(res.body());
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (Feature feature : fArray) {
            features.put(feature.getUid(), feature);
        }
        return features;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        Util.assertHasLength(groupName);
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_GROUPS, groupName);
        if (OK == res.statusCode()) {
            return true;
        }
        if (NOT_FOUND == res.statusCode()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of group , an HTTP error " + res.statusCode() + OCCURED);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        HttpResponse<String> res = getConnection().get(RESOURCE_STORE, RESOURCE_GROUPS);
        if (OK != res.statusCode()) {
            throw new FeatureAccessException("Cannot read groups, an HTTP error " + res.statusCode() + OCCURED);
        }
        try {
            List<Map<String, Object>> groupList = OBJECT_MAPPER.readValue(res.body(),
                    new TypeReference<List<Map<String, Object>>>() {});
            Set<String> groupNames = new HashSet<String>();
            for (Map<String, Object> currentGroup : groupList) {
                groupNames.add((String) currentGroup.get("groupName"));
            }
            return groupNames;
        } catch (com.fasterxml.jackson.core.JsonProcessingException e) {
            throw new FeatureAccessException("Cannot parse groups from API response", e);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void clear() {
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, STORE_CLEAR);
        if (OK != res.statusCode()) {
            throw new FeatureAccessException("Cannot clear feature store - " + res.statusCode());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        HttpResponse<String> res = getConnection().post(RESOURCE_STORE, STORE_CREATESCHEMA);
        if (OK != res.statusCode()) {
            throw new FeatureAccessException("Cannot create feature store - " + res.statusCode());
        }
    }

    /**
     * Getter accessor for attribute 'url'.
     *
     * @return current value of 'url'
     */
    public String getUrl() {
        return url;
    }

    /**
     * Setter accessor for attribute 'url'.
     * @param url new value for 'url '
     */
    public void setUrl(String url) {
        this.url = url;
        this.connection = null;
    }

}
