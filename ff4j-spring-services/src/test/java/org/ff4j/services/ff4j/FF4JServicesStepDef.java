package org.ff4j.services.ff4j;

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
import org.ff4j.security.AbstractAuthorizationManager;
import org.ff4j.services.AbstractStepDef;
import org.ff4j.services.FF4jServices;
import org.ff4j.services.FeatureServices;
import org.ff4j.services.domain.FeatureApiBean;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.*;

import static org.ff4j.services.utils.JsonUtils.GSON;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FF4JServicesStepDef extends AbstractStepDef {

    @Autowired
    private FF4jServices ff4jServices;
    @Autowired
    private FeatureServices featureServices;

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

    @Given("^the authorization manager is cleared$")
    public void the_authorization_manager_is_cleared() throws Throwable {
        ff4j.setAuthorizationsManager(null);
    }

    @Given("^the feature store has the following security information$")
    public void the_feature_store_has_the_following_security_information(List<TestAuthorizationsManager> authorizationsManagers) throws Throwable {
        int authManagersSize = authorizationsManagers.size();
        if (authManagersSize == 1) {
            ff4j.setAuthorizationsManager(authorizationsManagers.get(0));
        } else {
            throw new AssertionError("there should and can be only one AuthorizationManager");
        }
    }

    @When("^the user requests for status$")
    public void the_user_requests_for_status() throws Throwable {
        actualResponse = ff4jServices.getStatus();
    }

    @When("^the user requests for security$")
    public void the_user_requests_for_security() throws Throwable {
        try {
            actualResponse = ff4jServices.getSecurityInfo();
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to check if the feature is flipped with feature uid as \"([^\"]*)\"$")
    public void the_user_requests_to_check_if_the_feature_is_flipped_with_feature_uid_as(String featureUID) throws Throwable {
        try {
            actualResponse = ff4jServices.check(featureUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests for a feature by feature id as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_feature_id_as(String featureUID) throws Throwable {
        try {
            actualResponse = featureServices.getFeature(featureUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to check if the feature is flipped with feature uid as \"([^\"]*)\" and parameters$")
    public void the_user_requests_to_check_if_the_feature_is_flipped_with_feature_uid_as_and_parameters(String featureUID, Map<String, String> params) throws Throwable {
        Map<String, String> hashedParams = new HashMap<String, String>(params);
        Set<String> keys = hashedParams.keySet();
        for (String key : keys) {
            hashedParams.replace(key, hashedParams.get(key).replace("or", "|"));
        }
        try {
            actualResponse = ff4jServices.check(featureUID, hashedParams);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to create or update a feature with feature id as \"([^\"]*)\" and feature spec as$")
    public void the_user_requests_to_create_or_update_a_feature_with_feature_id_as_and_feature_spec_as(String featureUID, String featureSpec) throws Throwable {
        FeatureApiBean featureApiBean = GSON.fromJson(featureSpec, FeatureApiBean.class);
        try {
            actualResponse = featureServices.createOrUpdateFeature(featureUID, featureApiBean);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @Then("^feature is updated$")
    public void feature_is_updated() throws Throwable {
        assertUpdated();
    }

    @Then("^the user gets a response true$")
    public void the_user_gets_a_response_true() throws Throwable {
        assertTrue();
    }

    @Then("^the user gets a response false$")
    public void the_user_gets_a_response_false() throws Throwable {
        assertFalse();
    }

    @Then("^the user gets an exception \"([^\"]*)\"$")
    public void the_user_gets_an_exception(String className) throws Throwable {
        assertException(className);
    }

    @Then("^the user gets the response as$")
    public void the_user_gets_the_response_as(String expectedResponse) throws Throwable {
        assertLenientResponse(expectedResponse);
    }

    private class TestAuthorizationsManager extends AbstractAuthorizationManager {
        private String currentUserPermissions;

        private String allPermissions;

        private String currentUserName;

        @Override
        public String getCurrentUserName() {
            return currentUserName;
        }

        @Override
        public Set<String> getCurrentUserPermissions() {
            return StringUtils.isBlank(currentUserPermissions) ? null : new HashSet<String>(Arrays.asList(currentUserPermissions.split(",")));
        }

        @Override
        public Set<String> listAllPermissions() {
            return StringUtils.isBlank(allPermissions) ? null : new HashSet<String>(Arrays.asList(allPermissions.split(",")));
        }
    }
}


