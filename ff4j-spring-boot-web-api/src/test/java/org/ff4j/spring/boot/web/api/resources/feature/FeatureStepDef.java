package org.ff4j.spring.boot.web.api.resources.feature;

/*
 * #%L
 * ff4j-spring-boot-web-api
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

import com.google.gson.Gson;
import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.ff4j.core.Feature;
import org.ff4j.services.domain.FeatureApiBean;
import org.ff4j.spring.boot.web.api.resources.AbstractStepDef;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureStepDef extends AbstractStepDef {

    @Before
    @Override
    public void init() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(context).build();
    }

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

    @When("^the user requests for a feature by \"([^\"]*)\" by \"([^\"]*)\" http method and content type as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_by_http_method_and_content_type_as(String path, String httpMethod, String contentType) throws Throwable {
        constructRequestBuilder(path, httpMethod, contentType);
    }

    @When("^the user requests for a feature by \"([^\"]*)\" appended with \"([^\"]*)\" by \"([^\"]*)\" http method and content type as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_appended_with_by_http_method_and_content_type_as(String path, String appendToPath, String httpMethod, String contentType) throws Throwable {
        constructRequestBuilder(path + appendToPath, httpMethod, contentType);
    }

    @When("^request body as$")
    public void request_body_as(String requestBody) throws Throwable {
        setRequestBody(requestBody);
    }


    @Then("^the user gets an error response with code \"([^\"]*)\" and error message as \"([^\"]*)\"$")
    public void the_user_gets_an_error_response_with_code_and_error_message_as(int statusCode, String expectedErrorResponse) throws Throwable {
        assertErrorCodeAndMessage(statusCode, expectedErrorResponse);
    }

    @Then("^the user gets the response with response code as (\\d+) and content as \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\", \"([^\"]*)\" and \"([^\"]*)\"$")
    public void the_user_gets_the_response_with_response_code_as_and_content_as_and(int responseCode, String expectedUid, String expectedEnabled, String expectedDescription, String expectedGroup, String expectedPermissions) throws Throwable {
        assertStatus(responseCode);
        FeatureApiBean featureApiBean = new FeatureApiBean();
        featureApiBean.setUid(expectedUid);
        featureApiBean.setEnable(Boolean.valueOf(expectedEnabled));
        featureApiBean.setDescription(expectedDescription);
        featureApiBean.setGroup(expectedGroup);
        featureApiBean.setPermissions(Arrays.asList(expectedPermissions.split(",")));
        assertJsonResponse(new Gson().toJson(featureApiBean));
    }

    @Then("^the user gets the response with response code \"([^\"]*)\"$")
    public void the_user_gets_the_response_with_response_code(int expectedStatusCode) throws Throwable {
        assertStatus(expectedStatusCode);
    }

    @Then("^the response body as$")
    public void the_response_body_as(String expectedResponse) throws Throwable {
        assertJsonResponse(expectedResponse);
    }

    @Then("^the response body has content to be \"([^\"]*)\"$")
    public void the_response_body_has_content_to_be(String expectedResponse) throws Throwable {
        assertContent(expectedResponse);
    }
}
