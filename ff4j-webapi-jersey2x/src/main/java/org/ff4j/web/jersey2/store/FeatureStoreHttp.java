package org.ff4j.web.jersey2.store;

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

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.core.Feature;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.store.AbstractFeatureStore;
import org.ff4j.utils.Util;
import org.ff4j.web.api.resources.domain.FeatureApiBean;
import org.ff4j.web.api.utils.ClientHttpUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Implementation of store using {@link HttpClient} connection.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttp extends AbstractFeatureStore {

    /** logger for this class. */
    private final Logger log = LoggerFactory.getLogger(getClass());

    /** String constants */
    private static final String OCCURED = " occured.";
    
    /** constant. */
    private static final String CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR = "Cannot grant role on feature, an HTTP error ";
    
    /** Jersey Client. */
    protected Client jerseyClient = null;
    
    /** Property to get url ROOT. */
    private String url = null;
    
    /** header parameter to add if secured mode enabled. */
    private String authorizationHeaderValue = null;

    /** Target jersey resource. */
    private WebTarget storeWebRsc = null;

    /** Target jersey resource. */
    private WebTarget groupsWebRsc = null;

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
     * Initilialization of jersey.
     *
     * @return
     *      target Jersey
     */
    public Client getJerseyClient() {
        if (this.jerseyClient == null) {
            this.jerseyClient = ClientHttpUtils.buildJerseyClient();
        }
        return jerseyClient;
    }

    /**
     * Get access to store web resource.
     * 
     * @return target web resource
     */
    private WebTarget getStore() {
        Util.assertNotNull(url);
        if (storeWebRsc == null) {
            storeWebRsc = getJerseyClient().target(url).path(RESOURCE_STORE).path(RESOURCE_FEATURES);
        }
        return storeWebRsc;
    }

    /**
     * Get access to groups web resource.
     * 
     * @return target web resource
     */
    private WebTarget getGroups() {
        Util.assertNotNull(url);
        if (groupsWebRsc == null) {
            groupsWebRsc = getJerseyClient().target(url).path(RESOURCE_STORE).path(RESOURCE_GROUPS);
        }
        return groupsWebRsc;
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String uid) {
        Util.assertHasLength(uid);
        Response cRes = ClientHttpUtils.invokeGetMethod(
                getStore().path(uid), authorizationHeaderValue);
        log.info(String.valueOf(getStore().path(uid)));
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        } else if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Error when reaching API code:[" + cRes.getStatus() + "] MSG:" + cRes.getStatusInfo());
        }
        return parseFeature(cRes.readEntity(String.class));
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String uid) {
        Util.assertHasLength(uid);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getStore().path(uid).path(OPERATION_ENABLE), authorizationHeaderValue);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String uid) {
        Util.assertHasLength(uid);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getStore().path(uid).path(OPERATION_DISABLE), authorizationHeaderValue);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        Util.assertHasLength(uid);
        Response cRes = ClientHttpUtils.invokeGetMethod(getStore().path(uid), authorizationHeaderValue);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of feature, an HTTP error " + cRes.getStatus() + " occured : " + cRes.getEntity());
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
        /* Now can process upsert through PUT HTTP method
        Response cRes = getStore().path(fp.getUid())//
                .request(MediaType.APPLICATION_JSON) //
                .put(Entity.entity(new FeatureApiBean(fp), MediaType.APPLICATION_JSON));*/
        Response cRes = ClientHttpUtils
                            .createRequest(getStore().path(fp.getUid()), authorizationHeaderValue, null)
                            .put(Entity.entity(new FeatureApiBean(fp), MediaType.APPLICATION_JSON));
        
        // Check response code CREATED or raised error
        if (Status.CREATED.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        Response cRes = ClientHttpUtils.invokeGetMethod(getStore(), authorizationHeaderValue);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read features, an HTTP error " + cRes.getStatus() + OCCURED);
        }
       
        String resEntity = (String) cRes.readEntity(String.class);
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
        Util.assertHasLength(uid);
        Response cRes = ClientHttpUtils
                .invokeDeleteMethod(getStore().path(uid), authorizationHeaderValue);
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
        if (fp == null) {
            throw new IllegalArgumentException("Feature cannot be null nor empty");
        }
        if (!exist(fp.getUid())) {
            throw new FeatureNotFoundException(fp.getUid());
        }
        Response cRes = ClientHttpUtils
                .createRequest(getStore().path(fp.getUid()), authorizationHeaderValue, null)
                .put(Entity.entity(new FeatureApiBean(fp), MediaType.APPLICATION_JSON));
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot update feature, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        Util.assertHasLength(uid, roleName);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getStore().path(uid).path(OPERATION_GRANTROLE).path(roleName), authorizationHeaderValue);
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
        Util.assertHasLength(uid, roleName);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getStore().path(uid).path(OPERATION_REMOVEROLE).path(roleName), authorizationHeaderValue);
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
        Util.assertHasLength(uid, groupName);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getStore().path(uid).path(OPERATION_ADDGROUP).path(groupName), authorizationHeaderValue);
       
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
        Util.assertHasLength(uid, groupName);
        Response cRes = ClientHttpUtils.invokePostMethod(getStore()
                .path(uid)
                .path(OPERATION_REMOVEGROUP)
                .path(groupName), authorizationHeaderValue);
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
        Util.assertHasLength(groupName);
        Response cRes = ClientHttpUtils.invokePostMethod(getGroups()
                .path(groupName)
                .path(OPERATION_ENABLE), authorizationHeaderValue);
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
        Util.assertHasLength(groupName);
        Response cRes = ClientHttpUtils.invokePostMethod(getGroups()
                .path(groupName)
                .path(OPERATION_DISABLE), authorizationHeaderValue);
        
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Map<String, Feature> readGroup(String groupName) {
        Util.assertHasLength(groupName);
        Response cRes = ClientHttpUtils.invokeGetMethod(getGroups().path(groupName), authorizationHeaderValue);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException(CANNOT_GRANT_ROLE_ON_FEATURE_AN_HTTP_ERROR + cRes.getStatus() + OCCURED);
        }
        String resEntity = cRes.readEntity(String.class);
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
        Util.assertHasLength(groupName);
        Response cRes = ClientHttpUtils.invokeGetMethod(getGroups().path(groupName), authorizationHeaderValue);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of group , an HTTP error " + cRes.getStatus() + OCCURED);
    }

    /** {@inheritDoc} */
    @SuppressWarnings("unchecked")
    @Override
    public Set<String> readAllGroups() {
        Response cRes = ClientHttpUtils.invokeGetMethod(getGroups(), authorizationHeaderValue);
        List < Map < String, String>> groupList = cRes.readEntity(List.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read groups, an HTTP error " + cRes.getStatus() + OCCURED);
        }
        Set < String > groupNames = new HashSet<String>();
        for (Map <String, String > currentGroup : groupList) {
            groupNames.add(currentGroup.get("groupName"));
        }
        return groupNames;
    }
    
    /** {@inheritDoc} */
    @Override
    public void clear() {
        Util.assertHasLength(url);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getJerseyClient().target(url)
                .path(RESOURCE_STORE)
                .path(STORE_CLEAR),authorizationHeaderValue);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot clear feature store - " + cRes.getStatus());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        Util.assertHasLength(url);
        Response cRes = ClientHttpUtils.invokePostMethod(
                getJerseyClient().target(url)
                .path(RESOURCE_STORE)
                .path(STORE_CREATESCHEMA), authorizationHeaderValue);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create feature store - " + cRes.getStatus());
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
    }   

}
