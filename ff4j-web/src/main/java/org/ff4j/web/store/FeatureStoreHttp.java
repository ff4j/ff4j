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

import java.util.Map;
import java.util.Set;

import org.ff4j.core.Feature;
import org.ff4j.core.FeatureStore;

import com.sun.jersey.api.client.Client;
import com.sun.jersey.api.client.ClientResponse;
import com.sun.jersey.api.client.WebResource;
import com.sun.jersey.api.client.config.ClientConfig;
import com.sun.jersey.api.client.config.DefaultClientConfig;

/**
 * Implementation of store using {@link HttpClient} connection.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FeatureStoreHttp implements FeatureStore {

    private static final String RELATIVE_PATH_STORE = "features";

    protected ClientConfig config;

    protected Client client;

    /** Property to get url ROOT. */
    private final String rootApiUrl = null;

    public FeatureStoreHttp(String rootApiUrl) {
        config = new DefaultClientConfig();
        client = Client.create(config);
    }

    private WebResource getStore() {
        return client.resource(rootApiUrl).path(RELATIVE_PATH_STORE);
    }

    /** {@inheritDoc} */
    @Override
    public Feature read(String featureUid) {
        return unMarshallFeature(getStore().path(featureUid).get(String.class));
    }



    /** {@inheritDoc} */
    @Override
    public void enable(String featureID) {
        WebResource wr = getStore().path(featureID);
        ClientResponse res = wr.get(ClientResponse.class);
        Feature f = read(featureID);
        // Enable avec full update

    }

    @Override
    public void disable(String fId) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean exist(String featId) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public void create(Feature fp) {
        // TODO Auto-generated method stub

    }



    @Override
    public Map<String, Feature> readAll() {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void delete(String fpId) {
        // TODO Auto-generated method stub

    }

    @Override
    public void update(Feature fp) {
        // TODO Auto-generated method stub

    }

    @Override
    public void grantRoleOnFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void removeRoleFromFeature(String flipId, String roleName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void enableGroup(String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void disableGroup(String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public boolean existGroup(String groupName) {
        // TODO Auto-generated method stub
        return false;
    }

    @Override
    public Map<String, Feature> readGroup(String groupName) {
        // TODO Auto-generated method stub
        return null;
    }

    @Override
    public void addToGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public void removeFromGroup(String featureId, String groupName) {
        // TODO Auto-generated method stub

    }

    @Override
    public Set<String> readAllGroups() {
        // TODO Auto-generated method stub
        return null;
    }

}
