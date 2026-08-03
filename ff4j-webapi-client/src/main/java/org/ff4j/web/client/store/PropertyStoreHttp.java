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

import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTIES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_PROPERTYSTORE;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.ff4j.web.FF4jWebConstants.STORE_CREATESCHEMA;

import java.net.http.HttpClient;
import java.net.http.HttpResponse;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.PropertyAccessException;
import org.ff4j.exception.PropertyAlreadyExistException;
import org.ff4j.exception.PropertyNotFoundException;
import org.ff4j.property.Property;
import org.ff4j.property.store.AbstractPropertyStore;
import org.ff4j.utils.Util;
import org.ff4j.utils.json.PropertyJsonParser;
import org.ff4j.web.client.utils.ClientHttpUtils;
import org.ff4j.web.client.utils.HttpConnection;

/**
 * Implementation of {@link org.ff4j.property.store.PropertyStore} invoking the FF4j WebAPI
 * through the JDK {@link HttpClient} (no JAX-RS / Jersey dependency).
 *
 * @author Cedrick LUNVEN (@clunven)
 */
public class PropertyStoreHttp extends AbstractPropertyStore {

    public static final String OCCURED = " occured.";

    /** Http status. */
    private static final int OK = 200;

    /** Http status. */
    private static final int CREATED = 201;

    /** Http status. */
    private static final int NO_CONTENT = 204;

    /** Http status. */
    private static final int NOT_FOUND = 404;

    /** Property to get url ROOT. */
    private String url = null;

    /** header parameter to add if secured mode enabled. */
    private String authorization = null;

    /** Http connection to target API. */
    private HttpConnection connection = null;

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
     * Lazy initialization of the http connection.
     *
     * @return target connection
     */
    public HttpConnection getConnection() {
        if (connection == null) {
            Util.assertHasLength(url);
            connection = new HttpConnection(url, authorization);
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
    public boolean existProperty(String name) {
        Util.assertHasLength(name);
        HttpResponse<String> res = getConnection().get(RESOURCE_PROPERTYSTORE, RESOURCE_PROPERTIES, name);
        if (OK == res.statusCode()) {
            return true;
        }
        if (NOT_FOUND == res.statusCode()) {
            return false;
        }
        throw new PropertyAccessException("Cannot check existence of property, an HTTP error " +
                res.statusCode() + " occured : " + res.body());
    }

    /** {@inheritDoc} */
    public <T> void createProperty(Property<T> value) {
        Util.assertNotNull(value);
        Util.assertHasLength(value.getName());
        if (existProperty(value.getName())) {
            throw new PropertyAlreadyExistException("Property already exist");
        }
        // Upsert through PUT HTTP method
        HttpResponse<String> res = getConnection().put(value.toJson(), RESOURCE_PROPERTYSTORE, RESOURCE_PROPERTIES, value.getName());
        // Check response code CREATED or raised error
        if (CREATED != res.statusCode()) {
            throw new FeatureAccessException("Cannot create properties, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Property<?> readProperty(String name) {
        if (name == null || name.isEmpty()) {
            throw new IllegalArgumentException("Property name cannot be null nor empty");
        }
        HttpResponse<String> res = getConnection().get(RESOURCE_PROPERTYSTORE, RESOURCE_PROPERTIES, name);
        if (NOT_FOUND == res.statusCode()) {
            throw new PropertyNotFoundException(name);
        }
        if (OK != res.statusCode()) {
            throw new PropertyAccessException("Cannot read property, an HTTP error " + res.statusCode() + OCCURED);
        }
        return PropertyJsonParser.parseProperty(res.body());
    }

    /** {@inheritDoc} */
    public void deleteProperty(String name) {
        Util.assertHasLength(name);
        HttpResponse<String> res = getConnection().delete(RESOURCE_PROPERTYSTORE, RESOURCE_PROPERTIES, name);
        if (NOT_FOUND == res.statusCode()) {
            throw new PropertyNotFoundException(name);
        }
        if (NO_CONTENT != res.statusCode()) {
            throw new PropertyAccessException("Cannot delete property, an HTTP error " + res.statusCode() + OCCURED);
        }
    }

    /** {@inheritDoc} */
    public Map<String, Property<?>> readAllProperties() {
        HttpResponse<String> res = getConnection().get(RESOURCE_PROPERTYSTORE, RESOURCE_PROPERTIES);
        if (OK != res.statusCode()) {
            throw new PropertyAccessException("Cannot read properties, an HTTP error " + res.statusCode() + OCCURED);
        }
        Property<?>[] pArray = PropertyJsonParser.parsePropertyArray(res.body());
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
        HttpResponse<String> res = getConnection().post(RESOURCE_PROPERTYSTORE, STORE_CLEAR);
        if (OK != res.statusCode()) {
            throw new PropertyAccessException("Cannot clear property store - " + res.statusCode());
        }
    }

    /** {@inheritDoc} */
    @Override
    public void createSchema() {
        HttpResponse<String> res = getConnection().post(RESOURCE_PROPERTYSTORE, STORE_CREATESCHEMA);
        if (OK != res.statusCode()) {
            throw new PropertyAccessException("Cannot create property store - " + res.statusCode());
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
