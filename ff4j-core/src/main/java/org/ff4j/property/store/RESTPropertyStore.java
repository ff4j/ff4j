package org.ff4j.property.store;

/*
 * #%L
 * ff4j-core
 * %%
 * Copyright (C) 2013 - 2017 FF4J
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

import org.ff4j.property.Property;
import org.ff4j.property.PropertyString;
import org.ff4j.store.client.ApiClient;
import org.ff4j.store.client.ApiException;
import org.ff4j.store.client.api.PropertyResourceApi;
import org.ff4j.store.client.api.PropertyStoreResourceApi;
import org.ff4j.store.client.model.InlineResponse2003;

import java.net.URL;
import java.util.*;

/**
 * Created by peterdietz on 7/27/17.
 */
public class RESTPropertyStore extends AbstractPropertyStore {
    private PropertyStoreResourceApi propertyStoreResourceApi = null;
    private PropertyResourceApi propertyResourceApi = null;

    public RESTPropertyStore(URL apiURL) {
        ApiClient client = new ApiClient();
        client.setBasePath(apiURL.getPath());
        propertyStoreResourceApi = new PropertyStoreResourceApi(client);
        propertyResourceApi = new PropertyResourceApi(client);
    }


    @Override
    public boolean existProperty(String name) {
        try {
            List<String> properties = propertyStoreResourceApi.getPropertyStoreUsingGET().getProperties();
            return properties.contains(name);
        } catch (ApiException e) {
            System.err.println(e.getMessage());
        }

        return false;
    }

    @Override
    public <T> void createProperty(Property<T> value) {
        System.out.println("Not Implemented");
    }

    @Override
    public Property<?> readProperty(String name) {
        try {
            InlineResponse2003 response = propertyResourceApi.getPropertyUsingGET(name);
            Property<?> property = new PropertyString(response.getName(), response.getValue(), new HashSet<String>(response.getFixedValues()));
            return property;
        } catch (ApiException e) {
            System.err.println(e.getMessage());
        }

        return null;
    }

    @Override
    public void deleteProperty(String name) {
        System.out.println("Not Implemented");
    }

    @Override
    public Map<String, Property<?>> readAllProperties() {
        Map<String,Property<?>> propertyMap = new HashMap<String, Property<?>>();
        try {
            List<String> properties = propertyStoreResourceApi.getPropertyStoreUsingGET().getProperties();
            for(String property : properties) {
                propertyMap.put(property, readProperty(property));
            }
        } catch (ApiException e) {
            System.err.println(e.getMessage());
        }
        return propertyMap;
    }

    @Override
    public Set<String> listPropertyNames() {
        try {
            List<String> propertyList = propertyStoreResourceApi.getPropertyStoreUsingGET().getProperties();
            Set<String> propertySet = new HashSet<String>(propertyList);
            return propertySet;
        } catch (ApiException e) {
            System.err.println(e.getMessage());
        }

        return null;
    }

    @Override
    public void clear() {
        System.out.println("Not Implemented");
    }
}
