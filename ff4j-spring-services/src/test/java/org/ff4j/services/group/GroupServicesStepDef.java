package org.ff4j.services.group;

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
import org.ff4j.services.AbstractStepDef;
import org.ff4j.services.FeatureServices;
import org.ff4j.services.GroupServices;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class GroupServicesStepDef extends AbstractStepDef {

    @Autowired
    private GroupServices groupServices;
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

    @When("^the user requests for group \"([^\"]*)\"$")
    public void the_user_requests_for_group(String groupName) throws Throwable {
        try {
            actualResponse = groupServices.getFeaturesByGroup(groupName);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to enable group \"([^\"]*)\"$")
    public void the_user_requests_to_enable_group(String groupName) throws Throwable {
        try {
            groupServices.enableGroup(groupName);
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

    @When("^the user requests to disable group \"([^\"]*)\"$")
    public void the_user_requests_to_disable_group(String groupName) throws Throwable {
        try {
            groupServices.disableGroup(groupName);
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

