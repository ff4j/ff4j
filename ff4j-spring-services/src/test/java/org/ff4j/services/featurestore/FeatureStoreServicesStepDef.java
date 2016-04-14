package org.ff4j.services.featurestore;

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
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.core.Feature;
import org.ff4j.services.AbstractStepDef;
import org.ff4j.services.FeatureStoreServices;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Arrays;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureStoreServicesStepDef extends AbstractStepDef {

    @Autowired
    private FeatureStoreServices featureStoreServices;

    @Given("^the feature store is cleared$")
    public void the_feature_store_is_cleared() throws Throwable {
        clearFeatureStore();
    }

    @Given("^the following features exists in the feature store$")
    public void the_following_features_exists_in_the_feature_store(List<FeaturePojo> features) throws Throwable {
        createFeatures(features);
    }

    @Given("^the feature store is cached$")
    public void the_feature_store_is_cached() throws Throwable {
        FF4jCacheProxy proxy = new FF4jCacheProxy(ff4j.getFeatureStore(), null, new InMemoryCacheManager());
        ff4j.setFeatureStore(proxy);
    }

    @Given("^the following features are cached$")
    public void the_following_features_are_cached(List<FeaturePojo> features) throws Throwable {
        for (FeaturePojo featurePojo : features) {
            Feature feature = new Feature(featurePojo.getUid(), Boolean.valueOf(featurePojo.getEnable()),
                    featurePojo.getDescription(), featurePojo.getGroup(),
                    Arrays.asList(featurePojo.getPermissions().split(",")));
            ((FF4jCacheProxy) ff4j.getFeatureStore()).getCacheManager().putFeature(feature);
        }
    }

    @When("^the user requests to get the cached feature store$")
    public void the_user_requests_to_get_the_cached_feature_store() throws Throwable {
        try {
            actualResponse = featureStoreServices.getFeaturesFromCache();
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests for the feature store$")
    public void the_user_requests_for_the_feature_store() throws Throwable {
        actualResponse = featureStoreServices.getFeatureStore();
    }

    @When("^the user requests for all the features from the feature store$")
    public void the_user_requests_for_all_the_features_from_the_feature_store() throws Throwable {
        actualResponse = featureStoreServices.getAllFeatures();
    }

    @When("^the user requests for all the groups from the feature store$")
    public void the_user_requests_for_all_the_groups_from_the_feature_store() throws Throwable {
        actualResponse = featureStoreServices.getAllGroups();
    }

    @When("^the user requests to delete all the features from the feature store$")
    public void the_user_requests_to_delete_all_the_features_from_the_feature_store() throws Throwable {
        featureStoreServices.deleteAllFeatures();
    }

    @When("^the user requests to clear the cached feature store$")
    public void the_user_requests_to_clear_the_cached_feature_store() throws Throwable {
        try {
            featureStoreServices.clearCachedFeatureStore();
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
        assertStrictResponse(expectedResponse);
    }
}


