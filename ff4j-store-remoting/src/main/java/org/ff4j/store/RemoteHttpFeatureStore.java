package org.ff4j.store;

/*
 * #%L
 * RemoteHttpFeatureStore.java (ff4j-store-remoting) by Cedrick LUNVEN
 * %%
 * Copyright (C) 2013 Ff4J
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

import java.io.IOException;
import java.util.Map;

import org.apache.http.HttpResponse;
import org.apache.http.client.ClientProtocolException;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;
import org.ff4j.exception.FeatureAccessException;
import org.ff4j.exception.FeatureNotFoundException;
import org.ff4j.utils.FeatureJsonMarshaller;

/**
 * Implementation of {@link FeatureStore} to access features through HTTP.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class RemoteHttpFeatureStore implements FeatureStore {

    /** enable. */
    private static final String PATH_ENABLE = "/enable";

    /** disable. */
    private static final String PATH_DISABLE = "/disable";

    /** check if feature is exist. */
    private static final String PATH_EXIST = "/exist";

    /** read feature. */
    private static final String PATH_READ = "/read";

    /** Application json. */
    private static final String JSON = "application/json";

    /** target URL. */
    private String url;

    /** current http client. */
    private final DefaultHttpClient httpClient = new DefaultHttpClient();

    /**
     * Default constructor to allow IoC.
     */
    public RemoteHttpFeatureStore() {}

    /**
     * Parameteried Constructor to initialize attributes.
     * 
     * @param lUrl
     *            url to initialize url.
     */
    public RemoteHttpFeatureStore(String lUrl) {
        this.url = lUrl;
    }

    /**
     * Invole GET webServices to feature.
     * 
     * @param path
     *            target path
     * @param featureId
     *            current feature id
     * @return JSON output response
     */
    private String makeGetCall(String path, String featureId) {
        try {
            // Create request
            HttpGet getRequest = new HttpGet(url + path + "/" + featureId);
            getRequest.addHeader("accept", JSON);
            HttpResponse response = httpClient.execute(getRequest);

            java.util.Scanner s = new java.util.Scanner(response.getEntity().getContent()).useDelimiter("\\A");
            String output = s.hasNext() ? s.next() : "";
            httpClient.getConnectionManager().shutdown();

            // Handle Error
            if (response.getStatusLine().getStatusCode() == 404) {
                throw new FeatureNotFoundException(output);
            }
            if (response.getStatusLine().getStatusCode() != 200) {
                throw new FeatureAccessException("ErrorHTTP(" + response.getStatusLine().getStatusCode() + ") - " + output);
            }

            return output;

        } catch (ClientProtocolException cpe) {
            throw new FeatureAccessException("Cannot access remote HTTP Feature Store", cpe);
        } catch (IOException ioe) {
            throw new FeatureAccessException("Cannot read response from  HTTP Feature Store", ioe);
        }
    }

    /** {@inheritDoc} */
    @Override
    public void enable(String featureId) {
        this.makeGetCall(PATH_ENABLE, featureId);
    }

    /** {@inheritDoc} */
    @Override
    public void disable(String featureId) {
        this.makeGetCall(PATH_DISABLE, featureId);
    }

    /** {@inheritDoc} */
    @Override
    public boolean exist(String featId) {
        return Boolean.valueOf(this.makeGetCall(PATH_EXIST, featId));
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        return FeatureJsonMarshaller.unMarshallFeature(this.makeGetCall(PATH_READ, featureUid));
    }

    /** {@inheritDoc} */
    @Override
    public Map<String, Feature> readAll() {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /** {@inheritDoc} */
    @Override
    public void create(Feature fp) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /** {@inheritDoc} */
    @Override
    public void delete(String fpId) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /** {@inheritDoc} */
    @Override
    public void update(Feature fp) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /** {@inheritDoc} */
    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        throw new UnsupportedOperationException("Not yet implemented");
    }

    /** {@inheritDoc} */
    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        throw new UnsupportedOperationException("Not yet implemented");
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
     * 
     * @param url
     *            new value for 'url '
     */
    public void setUrl(String url) {
        this.url = url;
    }

}
