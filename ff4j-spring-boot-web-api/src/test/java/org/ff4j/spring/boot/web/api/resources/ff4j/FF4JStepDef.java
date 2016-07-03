package org.ff4j.spring.boot.web.api.resources.ff4j;

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

import cucumber.api.java.Before;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.apache.commons.lang3.StringUtils;
import org.ff4j.security.AbstractAuthorizationManager;
import org.ff4j.spring.boot.web.api.resources.AbstractStepDef;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FF4JStepDef extends AbstractStepDef {
    @Before
    @Override
    public void init() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(context).build();
    }

    @Given("^the feature store is cleared$")
    public void the_feature_store_is_cleared() throws Throwable {
        clearFeatureStore();
    }

    @Given("^the authorization manager is cleared$")
    public void theAuthorizationManagerIsCleared() throws Throwable {
        ff4j.setAuthorizationsManager(null);
    }

    @Given("^the following features exists in the feature store$")
    public void the_following_features_exists_in_the_feature_store(List<FeaturePojo> features) throws Throwable {
        createFeatures(features);
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


    @When("^the user requests for a feature by \"([^\"]*)\" by \"([^\"]*)\" http method and content type as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_by_http_method_and_content_type_as(String path, String httpMethod, String contentType) throws Throwable {
        constructRequestBuilder(path, httpMethod, contentType);
    }

    @When("^the following form param$")
    public void the_following_form_param(List<FormParam> formParams) throws Throwable {
        for (FormParam formParam : formParams) {
            requestBuilder.param(formParam.getName(), formParam.getValue().replace("or", "|"));
        }
    }

    @When("^request body as$")
    public void request_body_as(String requestBody) throws Throwable {
        setRequestBody(requestBody);
    }

    @Then("^the user gets the response with response code \"([^\"]*)\"$")
    public void the_user_gets_the_response_with_response_code(int expectedStatusCode) throws Throwable {
        assertStatus(expectedStatusCode);
    }

    @Then("^the user gets an error response with code \"([^\"]*)\" and error message as \"([^\"]*)\"$")
    public void the_user_gets_an_error_response_with_code_and_error_message_as(int statusCode, String expectedErrorResponse) throws Throwable {
        assertErrorCodeAndMessage(statusCode, expectedErrorResponse);
    }

    @Then("^the response body as$")
    public void the_response_body_as(String expectedResponse) throws Throwable {
        assertJsonResponse(expectedResponse);
    }

    @Then("^the response body has content to be \"([^\"]*)\"$")
    public void the_response_body_has_content_to_be(String expectedResponse) throws Throwable {
        assertContent(expectedResponse);
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

    private class FormParam {
        private String name;
        private String value;

        public String getName() {
            return name;
        }


        public String getValue() {
            return value;
        }
    }
}
