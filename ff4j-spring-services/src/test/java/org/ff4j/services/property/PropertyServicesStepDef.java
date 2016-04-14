package org.ff4j.services.property;

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
import org.ff4j.services.PropertyServices;
import org.ff4j.services.domain.PropertyApiBean;
import org.springframework.beans.factory.annotation.Autowired;

import java.util.List;

import static org.ff4j.services.utils.JsonUtils.GSON;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class PropertyServicesStepDef extends AbstractStepDef {

    @Autowired
    private PropertyServices propertyServices;

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

    @When("^the user requests for a property by property id as \"([^\"]*)\"$")
    public void the_user_requests_for_a_property_by_property_id_as(String propertyUID) throws Throwable {
        try {
            actualResponse = propertyServices.getProperty(propertyUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to create or update a property with property id as \"([^\"]*)\" and property spec as$")
    public void the_user_requests_to_create_or_update_a_property_with_property_id_as_and_property_spec_as(String propertyUID, String propertySpec) throws Throwable {
        PropertyApiBean propertyApiBean = GSON.fromJson(propertySpec, PropertyApiBean.class);
        try {
            actualResponse = propertyServices.createOrUpdateProperty(propertyUID, propertyApiBean);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to delete a property with property id as \"([^\"]*)\"$")
    public void the_user_requests_to_delete_a_property_with_property_id_as(String propertyUID) throws Throwable {
        try {
            propertyServices.deleteProperty(propertyUID);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @When("^the user requests to update a property with property id as \"([^\"]*)\" and property value as \"([^\"]*)\"$")
    public void the_user_requests_to_update_a_property_with_property_id_as_and_property_value_as(String propertyUID, String propertyValue) throws Throwable {
        try {
            propertyServices.updatePropertyName(propertyUID, propertyValue);
        } catch (Throwable t) {
            exception = t;
        }
    }

    @Then("^property is created$")
    public void property_is_created() throws Throwable {
        assertCreated();
    }

    @Then("^property is updated$")
    public void property_is_updated() throws Throwable {
        assertUpdated();
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


