package org.ff4j.spring.boot.web.api.resources.propertystore;

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
import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.cache.InMemoryCacheManager;
import org.ff4j.property.Property;
import org.ff4j.spring.boot.web.api.resources.AbstractStepDef;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class PropertyStoreStepDef extends AbstractStepDef {
    @Before
    @Override
    public void init() {
        this.mockMvc = MockMvcBuilders.webAppContextSetup(context).build();
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

    @When("^the user requests for a feature by \"([^\"]*)\" by \"([^\"]*)\" http method and content type as \"([^\"]*)\"$")
    public void the_user_requests_for_a_feature_by_by_http_method_and_content_type_as(String path, String httpMethod, String contentType) throws Throwable {
        constructRequestBuilder(path, httpMethod, contentType);
    }

    @When("^request body as$")
    public void request_body_as(String requestBody) throws Throwable {
        setRequestBody(requestBody);
    }

    @Then("^the user gets the response with response code \"([^\"]*)\"$")
    public void the_user_gets_the_response_with_response_code(int expectedStatusCode) throws Throwable {
        assertStatus(expectedStatusCode);
    }

    @Then("^the response body as$")
    public void the_response_body_as(String expectedResponse) throws Throwable {
        assertJsonResponse(expectedResponse);
    }

    @Then("^the user gets an error response with code \"([^\"]*)\" and error message as \"([^\"]*)\"$")
    public void the_user_gets_an_error_response_with_code_and_error_message_as(int statusCode, String expectedErrorResponse) throws Throwable {
        assertErrorCodeAndMessage(statusCode, expectedErrorResponse);
    }

    @Then("^the response body has content to be \"([^\"]*)\"$")
    public void the_response_body_has_content_to_be(String expectedResponse) throws Throwable {
        assertContent(expectedResponse);
    }
}
