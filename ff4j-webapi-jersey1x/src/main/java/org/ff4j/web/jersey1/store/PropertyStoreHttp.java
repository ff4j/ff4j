package org.ff4j.web.jersey1.store;

import static org.ff4j.web.FF4jWebConstants.HEADER_AUTHORIZATION;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTIES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTYSTORE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_STORE;
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

import javax.ws.rs.core.MediaType;
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
import org.ff4j.web.api.security.ClientHttpJersey1Utils;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;

/**
 * Implementation of the store with REST.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class PropertyStoreHttp extends AbstractPropertyStore {

    public static final String OCCURED = " occured.";
    /** Jersey Client. */
    protected Client jersey1xClient = null;

    /** Property to get url ROOT. */
    private String url = null;
    
    /** header parameter to add if secured mode enabled. */
    private String authorization = null;

    /** Target jersey resource. */
    private WebResource storeWebRsc = null;
    
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
    public PropertyStoreHttp(String rootApiUrl, String username, String password) {
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
            storeWebRsc = getJerseyClient().resource(url).path(RESOURCE_PROPERTYSTORE).path(RESOURCE_PROPERTIES);
            if (null != authorization) {
                storeWebRsc.header(HEADER_AUTHORIZATION, authorization);
            }
        }
        return storeWebRsc;
    }
    
    /** {@inheritDoc} */
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        ClientResponse cRes = getStore().path(name).get(ClientResponse.class);
        if (Status.OK.getStatusCode() == cRes.getStatus()) {
            return true;
        }
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            return false;
        }
        throw new PropertyAccessException("Cannot check existence of property, an HTTP error " + 
                cRes.getStatus() + " occured : " + cRes.getEntityInputStream());
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
            throw new FeatureAccessException("Cannot create schema for property store - " + cRes.getStatus());
        }
    }
    
    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> value) {
        Util.assertNotNull(value);
        Util.assertHasLength(value.getName());
        if (existProperty(value.getName())) {
            throw new PropertyAlreadyExistException("Property already exist");
        }
        ClientResponse cRes = getStore().path(value.getName())//
                .type(MediaType.APPLICATION_JSON) //
                .put(ClientResponse.class, new PropertyApiBean(value));
        
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
        ClientResponse cRes = getStore().path(name).get(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new PropertyNotFoundException(name);
        }
        return PropertyJsonParser.parseProperty(cRes.getEntity(String.class));
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        Util.assertHasLength(name);
        ClientResponse cRes = getStore().path(name).delete(ClientResponse.class);
        if (Status.NOT_FOUND.getStatusCode() == cRes.getStatus()) {
            throw new PropertyNotFoundException(name);
        }
        if (Status.NO_CONTENT.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot delete property, an HTTP error " + cRes.getStatus() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        ClientResponse cRes = getStore().get(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new PropertyAccessException("Cannot read properties, an HTTP error " + cRes.getStatus() + OCCURED);
        }
        String resEntity = cRes.getEntity(String.class);
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
        WebResource wr = getJerseyClient().resource(url).path(RESOURCE_PROPERTYSTORE).path(STORE_CLEAR);
        if (null != authorization) {
            wr.header(HEADER_AUTHORIZATION, authorization);
        }
        ClientResponse cRes = wr.post(ClientResponse.class);
        if (Status.OK.getStatusCode() != cRes.getStatus()) {
            throw new FeatureAccessException("Cannot clear property store - " + cRes.getStatus());
        }
    }
    
}
