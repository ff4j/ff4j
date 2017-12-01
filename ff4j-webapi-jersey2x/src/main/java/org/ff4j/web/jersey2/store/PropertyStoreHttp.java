package org.ff4j.web.jersey2.store;

import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.PARAM_AUTHKEY;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTIES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTYSTORE;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.ff4j.web.FF4jWebConstants.STORE_CREATESCHEMA;

/*
 * #%L
 * ff4j-webapi-jersey1x
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


import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import javax.ws.rs.client.Client;
import javax.ws.rs.client.Entity;
import javax.ws.rs.client.Invocation;
import javax.ws.rs.client.WebTarget;
import javax.ws.rs.core.MediaType;
import javax.ws.rs.core.Response;
import javax.ws.rs.core.Response.Status;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;
import org.ff4j.web.api.resources.domain.PropertyApiBean;
import org.ff4j.web.api.utils.ClientHttpUtils;
import org.glassfish.jersey.internal.util.Base64;

/**
 * Implementation of the store with REST.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreHttp extends AbstractPropertyStore {

    public static final String OCCURED = " occured.";
    
    /** Jersey Client. */
    protected Client jerseyClient = null;

    /** Property to get url ROOT. */
    private String url = null;
    
    /** header parameter to add if secured mode enabled. */
    private String authorization = null;

    /** Target jersey resource. */
    private WebTarget storeWebRsc = null;
    
    /**
     * Default construtor
     */
    public PropertyStoreHttp() {}

    /**
     * Initialization from URL.
     *
     * @param rootApiUrl
     *            target root URL
     */
    public PropertyStoreHttp(String rootApiUrl) {
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
    public PropertyStoreHttp(String rootApiUrl, String apiKey) {
        this(rootApiUrl);
        this.authorization = ClientHttpUtils.buildAuthorization4ApiKey(apiKey);
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
    public PropertyStoreHttp(String rootApiUrl, String username, String password) {
        this(rootApiUrl);
        this.authorization = ClientHttpUtils.buildAuthorization4UserName(username, password);
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
        if (storeWebRsc == null) {
            Util.assertHasLength(url);
            storeWebRsc = getJerseyClient().target(url).path(RESOURCE_PROPERTYSTORE).path(RESOURCE_PROPERTIES);
        }
        return storeWebRsc;
    }
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        Response cRes = ClientHttpUtils.invokeGetMethod(getStore().path(name), authorization);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new PropertyAccessException("Cannot check existence of property, an HTTP error " + 
                cRes.getStatus() + " occured : " + cRes.getEntity());
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> value) {
        Util.assertNotNull(value);
        Util.assertHasLength(value.getName());
        if (existProperty(value.getName())) {
            throw new PropertyAlreadyExistException("Property already exist");
        }
        /* Now can process upsert through PUT HTTP method
        Response cRes = getStore().path(value.getName())//
                .request(MediaType.APPLICATION_JSON)
                .put(Entity.entity(new PropertyApiBean(value), MediaType.APPLICATION_JSON));*/
        Response cRes = ClientHttpUtils
                .createRequest(getStore().path(value.getName()), authorization, null)
                .put(Entity.entity(new PropertyApiBean(value), MediaType.APPLICATION_JSON));
        
        // Check response code CREATED or raised error
        if (Status.CREATED.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot create properties, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Property name cannot be null nor empty");
        }
        Response cRes = ClientHttpUtils.invokeGetMethod(getStore().path(name), authorization);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new PropertyNotFoundException(name);
        }
        String resEntity = (String) cRes.readEntity(String.class);
        return PropertyJsonParser.parseProperty(resEntity);
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        Util.assertHasLength(name);
        Response cRes = ClientHttpUtils.invokeDeleteMethod(getStore().path(name), authorization);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new PropertyNotFoundException(name);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot delete property, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        Response cRes = ClientHttpUtils.invokeGetMethod(getStore(), authorization);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot read properties, an HTTP error " + cRes.getStatus() + OCCURED);
        }
        String resEntity = cRes.readEntity(String.class);
        Property<?>[] pArray = PropertyJsonParser.parsePropertyArray(resEntity);
        Map<String, Property<?>> properties = new HashMap<String, Property<?>>();
        for (Property<?> pName : pArray) {
            properties.put(pName.getName(), pName);
        }
        return properties;
    }

    /** {@inheritDoc} */
    public Set<String> listPropertyNames() {
        return readAllProperties().keySet();
    }

    /** {@inheritDoc} */
    public void clear() {
        Util.assertHasLength(url);
        WebTarget wr = getJerseyClient().target(url).path(RESOURCE_PROPERTYSTORE).path(STORE_CLEAR);
        Response cRes = post(wr);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot clear property store - " + cRes.getStatus());
        }
    }
    
    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        Util.assertHasLength(url);
        WebTarget wr = getJerseyClient().target(url).path(RESOURCE_PROPERTYSTORE).path(STORE_CREATESCHEMA);
        Response cRes = post(wr);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot clear property store - " + cRes.getStatus());
        }
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
        return " Basic " + new String(Base64.encodeAsString(username + ":" + password));
    }
    
    /**
     * Share header settings for invocations.
     *
     * @param webTarget
     *          target web
     * @return
     */
    private Response post(WebTarget webTarget) {
        Invocation.Builder invocationBuilder = webTarget.request(MediaType.APPLICATION_JSON_TYPE);
        if (null != authorization) {
            invocationBuilder.header(HEADER_AUTHORIZATION, authorization);
        }
        return invocationBuilder.post(Entity.text(""));
    } 
    
}
