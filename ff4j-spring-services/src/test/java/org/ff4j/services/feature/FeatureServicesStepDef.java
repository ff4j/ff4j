package org.ff4j.services.feature;

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
import org.ff4j.core.Feature;
import org.ff4j.services.AbstractStepDef;
import org.ff4j.services.FeatureServices;
import org.ff4j.services.domain.FeatureApiBean;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.Arrays;
import java.util.List;

import static org.assertj.core.api.Assertions.assertThat;
import static org.ff4j.services.utils.JsonUtils.GSON;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureServicesStepDef extends AbstractStepDef {

    @Autowired
    private FeatureServices featureServices;

    @Given("^the feature store is cleared$")
    public void the_feature_store_is_cleared() throws Throwable {
        clearFeatureStore();
    }

    @Given("^the feature with \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\" and \"([^\"]*)\" exists in the feature store$")
    public void the_feature_with_and_exists_in_the_feature_store(String uid, String enabled, String description, String group, String csvPermissions) throws Throwable {
        Feature feature = new Feature(uid, Boolean.valueOf(enabled), description, group, Arrays.asList(csvPermissions.split(",")));
        createFeature(feature);
    }

    @Given("^the following features exists in the feature store$")
    public void the_following_features_exists_in_the_feature_store(List<FeaturePojo> features) throws Throwable {
        createFeatures(features);
    }

    @When("^the user requests for a feature by feature id as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_feature_id_as(String featureUID) throws Throwable {
        try {
            actualResponse = featureServices.getFeature(featureUID);
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

    @When("^the user requests to delete a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_delete_a_feature_with_feature_id_as(String featureUID) throws Throwable {
        try {
            featureServices.deleteFeature(featureUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to disable a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_disable_a_feature_with_feature_id_as(String featureUID) throws Throwable {
        try {
            featureServices.disableFeature(featureUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to enable a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_enable_a_feature_with_feature_id_as(String featureUID) throws Throwable {
        try {
            featureServices.enableFeature(featureUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to grant role \"([^\"]*)\" to a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_grant_role_to_a_feature_with_feature_id_as(String roleName, String featureUID) throws Throwable {
        try {
            featureServices.grantRoleToFeature(featureUID, roleName);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to remove role \"([^\"]*)\" to a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_remove_role_to_a_feature_with_feature_id_as(String roleName, String featureUID) throws Throwable {
        try {
            featureServices.removeRoleFromFeature(featureUID, roleName);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to add group \"([^\"]*)\" to a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_add_group_to_a_feature_with_feature_id_as(String groupName, String featureUID) throws Throwable {
        try {
            featureServices.addGroupToFeature(featureUID, groupName);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to remove group \"([^\"]*)\" to a feature with feature id as \"([^\"]*)\"$")
    public void the_user_requests_to_remove_group_to_a_feature_with_feature_id_as(String groupName, String featureUID) throws Throwable {
        try {
            featureServices.removeGroupFromFeature(featureUID, groupName);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @Then("^the user gets an exception \"([^\"]*)\"$")
    public void the_user_gets_an_exception(String className) throws Throwable {
        assertException(className);
    }

    @Then("^the user gets the response as \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\" and \"([^\"]*)\"$")
    public void the_user_gets_the_response_as_and(String expectedUid, String expectedEnabled, String expectedDescription, String expectedGroup, String expectedPermissions) throws Throwable {
        Feature expectedFeature = new Feature(expectedUid, Boolean.valueOf(expectedEnabled), expectedDescription, expectedGroup, Arrays.asList(expectedPermissions.split(",")));
        FeatureApiBean expectedFeatureApiBean = new FeatureApiBean(expectedFeature);
        assertThat(actualResponse).isEqualToComparingFieldByField(expectedFeatureApiBean);
    }

    @Then("^feature is created$")
    public void feature_is_created() throws Throwable {
        assertCreated();
    }

    @Then("^the user gets the response as$")
    public void the_user_gets_the_response_as(String expectedResponse) throws Throwable {
        FeatureApiBean featureApiBean = GSON.fromJson(expectedResponse, FeatureApiBean.class);
        assertThat(actualResponse).isEqualToComparingOnlyGivenFields(featureApiBean);
    }

    @Then("^feature is updated$")
    public void feature_is_updated() throws Throwable {
        assertUpdated();
    }
}


