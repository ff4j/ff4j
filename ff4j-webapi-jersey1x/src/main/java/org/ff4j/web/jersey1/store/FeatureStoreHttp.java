package org.ff4j.web.jersey1.store;

/*
 * #%L
 * ff4j-web
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

import static org.ff4j.utils.json.FeatureJsonParser.parseFeature;
import static org.ff4j.utils.json.FeatureJsonParser.parseFeatureArray;
import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
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

import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response.Status;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.ff4j.web.api.resources.domain.GroupDescApiBean;
import org.ff4j.web.api.security.ClientHttpJersey1Utils;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.GenericType;
import com.sun.jersey.api.client.WebResource;

/**
 * Implementation of store using {@link HttpClient} connection.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttp extends AbstractFeatureStore {

    private static final String OCCURED = " occured.";

    private static final String GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY = "Groupname cannot be null nor empty";

    private static final String CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR = "Cannot grant role on feature, an HTTP error ";

    private static final String ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY = "roleName cannot be null nor empty";

    private static final String FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY = "Feature identifier cannot be null nor empty";

    /** Jersey Client. */
    protected Client jersey1xClient = null;

    /** Property to get url ROOT. */
    private String url = null;
    
    /** header parameter to add if secured mode enabled. */
    private String authorization = null;

    /** Target jersey resource. */
    private WebResource storeWebRsc = null;

    /** Target jersey resource. */
    private WebResource groupsWebRsc = null;

    /**
     * Default construtor
     */
    public FeatureStoreHttp() {}

    /**
     * Initialization from URL.
     *
     * @param rootApiUrl
     *            target root URL
     */
    public FeatureStoreHttp(String rootApiUrl) {
        this.url = rootApiUrl;
    }
    
    /**
     * Authentication through APIKEY.
     *
     * @param rootApiUrl
     *      target url
     * @param apiKey
     *      target api
     */
    public FeatureStoreHttp(String rootApiUrl, String apiKey) {
        this(rootApiUrl);
        this.authorization = ClientHttpJersey1Utils.buildAuthorization4ApiKey(apiKey);
    }
    
    /**
     * Authentication through login/password.
     *
     * @param rootApiUrl
     *      target url
     * @param username
     *      target username
     * @param password
     *      target password
     */
    public FeatureStoreHttp(String rootApiUrl, String username, String password) {
        this(rootApiUrl);
        this.authorization = ClientHttpJersey1Utils.buildAuthorization4UserName(username, password);
    }

    /**
     * Initilialization of jersey.
     *
     * @return
     *      target Jersey
     */
    public Client getJerseyClient() {
        if (this.jersey1xClient == null) {
            this.jersey1xClient = ClientHttpJersey1Utils.buildJersey1Client();
        }
        return jersey1xClient;
    }

    /**
     * Get access to store web resource.
     * 
     * @return target web resource
     */
    private WebResource getStore() {
        if (storeWebRsc == null) {
            Util.assertHasLength(url);
            storeWebRsc = getJerseyClient().resource(url).path(RESOURCE_STORE).path(RESOURCE_FEATURES);
            if (null != authorization) {
                storeWebRsc.header(HEADER_AUTHORIZATION, authorization);
            }
        }
        return storeWebRsc;
    }

    /**
     * Get access to groups web resource.
     * 
     * @return target web resource
     */
    private WebResource getGroups() {
        if (groupsWebRsc == null) {
            Util.assertHasLength(url);
            groupsWebRsc = getJerseyClient().resource(url).path(RESOURCE_STORE).path(RESOURCE_GROUPS);
            if (null != authorization) {
                groupsWebRsc.header(HEADER_AUTHORIZATION, authorization);
            }
        }
        return groupsWebRsc;
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).get(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        return parseFeature(cRes.getEntity(String.class));
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_ENABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_DISABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).get(ClientResponse.class);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of feature, an HTTP error " + cRes.getStatus() + " occured : " + cRes.getEntityInputStream());
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
        // Now can process upsert through PUT HTTP method
        ClientResponse cRes = getStore().path(fp.getUid())//
                .type(MediaType.APPLICATION_JSON) //
                .put(ClientResponse.class, new FeatureApiBean(fp));
        // Check response code CREATED or raised error
        if (Status.CREATED.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        ClientResponse cRes = getStore().get(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read features, an HTTP error " + cRes.getStatus() + OCCURED);
        }
        String resEntity = cRes.getEntity(String.class);
        Feature[] fArray = parseFeatureArray(resEntity);
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (Feature feature : fArray) {
            features.put(feature.getUid(), feature);
        }
        return features;
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).delete(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot delete feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        Util.assertNotNull(fp);
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        ClientResponse cRes = getStore().path(fp.getUid()) //
                .type(MediaType.APPLICATION_JSON)
                .put(ClientResponse.class, new FeatureApiBean(fp));
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot update feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException(ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_GRANTROLE).path(roleName).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException(ROLE_NAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_REMOVEROLE).path(roleName).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot remove role on feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_ADDGROUP).path(groupName).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot add feature to group, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException(FEATURE_IDENTIFIER_CANNOT_BE_NULL_NOR_EMPTY);
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getStore().path(uid).path(OPERATION_REMOVEGROUP).path(groupName).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.BAD_REQUEST.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot remove feature from group, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getGroups().path(groupName).path(OPERATION_ENABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getGroups().path(groupName).path(OPERATION_DISABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getGroups().path(groupName).get(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
        String resEntity = cRes.getEntity(String.class);
        Feature[] fArray = parseFeatureArray(resEntity);
        Map<String, Feature> features = new HashMap<String, Feature>();
        for (Feature feature : fArray) {
            features.put(feature.getUid(), feature);
        }
        return features;
    }

    /** {@inheritDoc} */
    @Override
    public boolean existGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException(GROUPNAME_CANNOT_BE_NULL_NOR_EMPTY);
        }
        ClientResponse cRes = getGroups().path(groupName).get(ClientResponse.class);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of group , an HTTP error " + cRes.getStatus() + OCCURED);
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        ClientResponse cRes = getGroups().get(ClientResponse.class);
        List<GroupDescApiBean> groupApiBeans = cRes.getEntity(new GenericType<List<GroupDescApiBean>>() {});
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read groups, an HTTP error " + cRes.getStatus() + OCCURED);
        }
        Set < String > groupNames = new HashSet<String>();
        for (GroupDescApiBean groupApiBean : groupApiBeans) {
            groupNames.add(groupApiBean.getGroupName());
        }
        return groupNames;
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        Util.assertHasLength(url);
        WebResource wr = getJerseyClient().resource(url).path(RESOURCE_STORE).path(STORE_CLEAR);
        if (null != authorization) {
            wr.header(HEADER_AUTHORIZATION, authorization);
        }
        ClientResponse cRes = wr.post(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot clear feature store - " + cRes.getStatus());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        Util.assertHasLength(url);
        WebResource wr = getJerseyClient().resource(url).path(RESOURCE_STORE).path(STORE_CREATESCHEMA);
        if (null != authorization) {
            wr.header(HEADER_AUTHORIZATION, authorization);
        }
        ClientResponse cRes = wr.post(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create schema for feature store - " + cRes.getStatus());
        }
    }

    /**
     * Getter accessor for attribute 'url'.
     *
     * @return
     *       current value of 'url'
     */
    public String getUrl() {
        return url;
    }

    /**
     * Setter accessor for attribute 'url'.
     * @param url
     * 		new value for 'url '
     */
    public void setUrl(String url) {
        this.url = url;
    }

}
