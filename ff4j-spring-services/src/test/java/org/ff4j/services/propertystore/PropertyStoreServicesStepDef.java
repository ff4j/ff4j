package org.ff4j.services.propertystore;

/*
 * #%L
 * ff4j-spring-services
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

import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.apache.commons.lang3.StringUtils;
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.property.Property;
import org.ff4j.services.AbstractStepDef;
import org.ff4j.services.PropertyStoreServices;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class PropertyStoreServicesStepDef extends AbstractStepDef {

    @Autowired
    private PropertyStoreServices propertyStoreServices;

    @Given("^the feature store is cleared$")
    public void the_feature_store_is_cleared() throws Throwable {
        clearFeatureStore();
    }

    @Given("^the following features exists in the feature store$")
    public void the_following_features_exists_in_the_feature_store(List<FeaturePojo> features) throws Throwable {
        createFeatures(features);
    }

    @Given("^the property store is cleared$")
    public void the_property_store_is_cleared() throws Throwable {
        clearPropertyStore();
    }

    @Given("^the following properties exists in the property store$")
    public void the_following_properties_exists_in_the_property_store(List<PropertyPojo> properties) throws Throwable {
        createProperties(properties);
    }

    @Given("^the property store is cached$")
    public void the_property_store_is_cached() throws Throwable {
        FF4jCacheProxy proxy = new FF4jCacheProxy(ff4j.getFeatureStore(), ff4j.getPropertiesStore(), new InMemoryCacheManager());
        ff4j.setPropertiesStore(proxy);
        ff4j.setFeatureStore(proxy);
    }

    @Given("^the following properties are cached$")
    public void the_following_properties_are_cached(List<PropertyPojo> properties) throws Throwable {
        for (PropertyPojo propertyPojo : properties) {
            Property<?> property = asProperty(propertyPojo.getName(), propertyPojo.getType(), propertyPojo.getValue(),
                    propertyPojo.getDescription(),
                    StringUtils.isNotBlank(propertyPojo.getFixedValueCSV()) ? new HashSet<String>(Arrays.asList(propertyPojo.getFixedValueCSV().split(","))) : null);
            ((FF4jCacheProxy) ff4j.getPropertiesStore()).getCacheManager().putProperty(property);
        }
    }

    @When("^the user requests for the property store$")
    public void the_user_requests_for_the_property_store() throws Throwable {
        actualResponse = propertyStoreServices.getPropertyStore();
    }

    @When("^the user requests for all the properties from the property store$")
    public void the_user_requests_for_all_the_properties_from_the_property_store() throws Throwable {
        actualResponse = propertyStoreServices.getAllProperties();
    }

    @When("^the user requests to delete all the properties from the property store$")
    public void the_user_requests_to_delete_all_the_properties_from_the_property_store() throws Throwable {
        propertyStoreServices.deleteAllProperties();
    }

    @When("^the user requests to get the cached property store$")
    public void the_user_requests_to_get_the_cached_property_store() throws Throwable {
        try {
            actualResponse = propertyStoreServices.getPropertiesFromCache();
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to clear the cached property store$")
    public void the_user_requests_to_clear_the_cached_property_store() throws Throwable {
        try {
            propertyStoreServices.clearCachedPropertyStore();
        } catch (Throwable t) {
            exception = t;
        }
    }

    @Then("^the user gets an exception \"([^\"]*)\"$")
    public void the_user_gets_an_exception(String className) throws Throwable {
        assertException(className);
    }

    @Then("^the user gets the response as$")
    public void the_user_gets_the_response_as(String expectedResponse) throws Throwable {
        assertLenientResponse(expectedResponse);
    }
}


