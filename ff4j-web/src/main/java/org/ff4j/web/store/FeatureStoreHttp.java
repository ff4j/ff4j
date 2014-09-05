package org.ff4j.web.store;

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

import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.core.MultivaluedMap;
import javax.ws.rs.core.Response.Status;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureAlreadyExistException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.exception.GroupNotFoundException;
import org.ff4j.web.api.FF4jWebConstants;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.core.util.Base64;
import com.sun.jersey.core.util.MultivaluedMapImpl;

/**
 * Implementation of store using {@link HttpClient} connection.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttp implements FeatureStore, FF4jWebConstants {

    /** Jersey Client. */
    protected Client client = null;

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
        this.authorization = buildAuthorization4ApiKey(apiKey);
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
        this.authorization = buildAuthorization4UserName(username, password);
    }

    /**
     * Initializing jerseyClient.
     */
    private void initJerseyClient() {
        if (client == null) {
            ClientConfig config = new DefaultClientConfig();
            client = Client.create(config);
        }
        if (url == null) {
            throw new IllegalArgumentException("Cannot initialialize Jersey Client : please provide store URL in 'url' attribute");
        }
    }

    /**
     * Get access to store web resource.
     * 
     * @return target web resource
     */
    private WebResource getStore() {
        if (storeWebRsc == null) {
            initJerseyClient();
            storeWebRsc = client.resource(url).path(RESOURCE_STORE).path(RESOURCE_FEATURES);
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
            initJerseyClient();
            groupsWebRsc = client.resource(url).path(RESOURCE_STORE).path(RESOURCE_GROUPS);
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
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
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
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
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
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        ClientResponse cRes = storeWebRsc.path(uid).path(OPERATION_DISABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String uid) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
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
        ClientResponse cRes = getStore().path(fp.getUid()).put(ClientResponse.class, fp.toString().getBytes());
        // Check response code CREATED or raised error
        if (Status.CREATED.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        ClientResponse cRes = getStore().get(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read features, an HTTP error " + cRes.getStatus() + " occured.");
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
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        ClientResponse cRes = getStore().path(uid).delete(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot delete feature, an HTTP error " + cRes.getStatus() + " occured.");
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
        ClientResponse cRes = getStore().path(fp.getUid()).put(ClientResponse.class, fp.toString().getBytes());
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot update feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_ROLENAME, roleName);
        ClientResponse cRes = getStore().path(uid).path(OPERATION_GRANTROLE).post(ClientResponse.class, formData);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot grant role on feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String uid, String roleName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (roleName == null || roleName.isEmpty()) {
            throw new IllegalArgumentException("roleName cannot be null nor empty");
        }
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_ROLENAME, roleName);
        ClientResponse cRes = getStore().path(uid).path(OPERATION_REMOVEROLE).post(ClientResponse.class, formData);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot remove role on feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void addToGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_GROUPNAME, groupName);
        ClientResponse cRes = getStore().path(uid).path(OPERATION_ADDGROUP).post(ClientResponse.class, formData);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot add feature to group, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void removeFromGroup(String uid, String groupName) {
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        if (uid == null || uid.isEmpty()) {
            throw new IllegalArgumentException("Feature identifier cannot be null nor empty");
        }
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        MultivaluedMap<String, String> formData = new MultivaluedMapImpl();
        formData.add(POST_PARAMNAME_GROUPNAME, groupName);
        ClientResponse cRes = getStore().path(uid).path(OPERATION_REMOVEGROUP).post(ClientResponse.class, formData);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            String errorMsg = cRes.getEntity(String.class);
            if (errorMsg.contains("group")) {
                throw new GroupNotFoundException(groupName);
            }
            throw new FeatureNotFoundException(uid);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot remove feature from group, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        ClientResponse cRes = getGroups().path(groupName).path(OPERATION_ENABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot grant role on feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public void disableGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        ClientResponse cRes = getGroups().path(groupName).path(OPERATION_DISABLE).post(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot grant role on feature, an HTTP error " + cRes.getStatus() + " occured.");
        }
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readGroup(String groupName) {
        if (groupName == null || groupName.isEmpty()) {
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        ClientResponse cRes = getGroups().path(groupName).get(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new GroupNotFoundException(groupName);
        }
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot grant role on feature, an HTTP error " + cRes.getStatus() + " occured.");
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
            throw new IllegalArgumentException("Groupname cannot be null nor empty");
        }
        ClientResponse cRes = getGroups().path(groupName).get(ClientResponse.class);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new FeatureAccessException("Cannot check existence of group , an HTTP error " + cRes.getStatus() + " occured.");
    }

    /** {@inheritDoc} */
    @Override
    public Set<String> readAllGroups() {
        ClientResponse cRes = getGroups().get(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot read groups, an HTTP error " + cRes.getStatus() + " occured.");
        }
        return readGroupList(cRes.getEntity(String.class));
    }

    /**
     * Convert JsonOutput to group set.
     * 
     * @param jsonOutput
     * @return
     */
    private Set<String> readGroupList(String jsonOutput) {
        // Remove brackets
        String resEntity = jsonOutput.substring(2, jsonOutput.length() - 1);
        // Split
        String[] features = resEntity.split("\\,");
        // Map as SET, remove quotes
        Set<String> groups = new HashSet<String>();
        for (String string : features) {
            groups.add(string.substring(1, string.indexOf(":") - 1));
        }
        return groups;
    }

    /** {@inheritDoc} */
    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder("{");
        sb.append("\"type\":\"" + this.getClass().getCanonicalName() + "\"");
        sb.append("\"target\":\"" + this.url + "\"");
        sb.append(",\"cached\":" + this.isCached());
        if (this.isCached()) {
            sb.append(",\"cacheProvider\":\"" + this.getCacheProvider() + "\"");
            sb.append(",\"cacheStore\":\"" + this.getCachedTargetStore() + "\"");
        }
        Set<String> myFeatures = readAll().keySet();
        sb.append(",\"numberOfFeatures\":" + myFeatures.size());
        sb.append(",\"features\":[");
        boolean first = true;
        for (String myFeature : myFeatures) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"" + myFeature + "\"");
        }
        Set<String> myGroups = readAllGroups();
        sb.append("],\"numberOfGroups\":" + myGroups.size());
        sb.append(",\"groups\":[");
        first = true;
        for (String myGroup : myGroups) {
            if (!first) {
                sb.append(",");
            }
            first = false;
            sb.append("\"" + myGroup + "\"");
        }
        sb.append("]");
        sb.append("}");
        return sb.toString();
    }

    // -------- Overrided in cache proxy --------------

    /** {@inheritDoc} */
    @Override
    public boolean isCached() {
        return false;
    }

    /** {@inheritDoc} */
    @Override
    public String getCacheProvider() {
        return null;
    }

    /** {@inheritDoc} */
    @Override
    public String getCachedTargetStore() {
        return null;
    }
    
    // ------- Static for authentication -------
    
    /**
     * Build Authorization header for final user.
     * @param username
     *      target username
     * @param password
     *      target password
     * @return
     *      target header
     */
    public static String buildAuthorization4UserName(String username, String password) {
        return " Basic " + new String(Base64.encode(username + ":" + password));
    }
    
    /**
     * Build Authorization header for technical user.
     * @param apiKey
     *      target apiKey
     * @return
     *      target header
     */
    public static String buildAuthorization4ApiKey(String apiKey) {
        return PARAM_AUTHKEY + "=" + apiKey;
    }

}
