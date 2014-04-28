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

import static org.ff4j.utils.FeatureJsonMarshaller.unMarshallFeature;

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
import org.ff4j.utils.FeatureJsonMarshaller;
import org.ff4j.web.api.FF4jWebApiConstants;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;
import com.sun.jersey.core.util.MultivaluedMapImpl;

/**
 * Implementation of store using {@link HttpClient} connection.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttp implements FeatureStore, FF4jWebApiConstants {

    /** Jersey Client. */
    protected Client client = null;

    /** Property to get url ROOT. */
    private String url = null;

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
        return unMarshallFeature(cRes.getEntity(String.class));
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
        throw new FeatureAccessException("Cannot check existence of feature, an HTTP error " + cRes.getStatus() + " occured.");
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
        Feature[] fArray = FeatureJsonMarshaller.unMarshallFeatureArray(resEntity);
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
        Feature[] fArray = FeatureJsonMarshaller.unMarshallFeatureArray(resEntity);
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

}
