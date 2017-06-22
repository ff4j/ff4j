package org.ff4j.spring.boot.web.api.resources;

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

import io.swagger.annotations.ApiOperation;
import io.swagger.annotations.ApiResponse;
import io.swagger.annotations.ApiResponses;
import org.ff4j.services.PropertyServices;
import org.ff4j.services.domain.PropertyApiBean;
import org.ff4j.spring.boot.web.api.utils.FeatureWebUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import static org.ff4j.services.constants.FeatureConstants.*;
import static org.ff4j.web.FF4jWebConstants.OPERATION_UPDATE;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.*;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_PROPERTIES_STORE_PROPERTIES + "/" + PATH_PARAM_NAME)
public class PropertyResource {
    @Autowired
    private PropertyServices propertyServices;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Read information about a property", response = PropertyApiBean.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "Information about property"),
            @ApiResponse(code = 404, message = "Property not found")})
    public PropertyApiBean getProperty(@PathVariable(value = PARAM_NAME) String propertyName) {
        return propertyServices.getProperty(propertyName);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(method = PUT, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Create or update a property", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 400, message = "Property name is blank (or) property name did not match with the requested property name to be created or updated"),
            @ApiResponse(code = 201, message = "Property has been created"),
            @ApiResponse(code = 202, message = "Property has been updated"),
            @ApiResponse(code = 204, message = "No content, no changes made to the feature")})
    public ResponseEntity createOrUpdateProperty(@PathVariable(value = PARAM_NAME) String propertyName, @RequestBody PropertyApiBean propertyApiBean) {
        return FeatureWebUtils.getBooleanResponseEntityByHttpStatus(propertyServices.createOrUpdateProperty(propertyName, propertyApiBean));
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(method = DELETE, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Delete a property", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 204, message = "No content, property is deleted"),
            @ApiResponse(code = 404, message = "Property not found")
    })
    public ResponseEntity deleteProperty(@PathVariable(value = PARAM_NAME) String propertyName) {
        propertyServices.deleteProperty(propertyName);
        return new ResponseEntity(NO_CONTENT);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_UPDATE + "/" + PATH_PARAM_VALUE, method = POST, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Update value of a property", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Property has been updated"),
            @ApiResponse(code = 404, message = "Property not found"),
            @ApiResponse(code = 400, message = "Invalid new value")})
    public ResponseEntity updatePropertyName(@PathVariable(value = PARAM_NAME) String propertyName, @PathVariable(value = PARAM_VALUE) String newPropertyName) {
        propertyServices.updatePropertyName(propertyName, newPropertyName);
        return new ResponseEntity(HttpStatus.ACCEPTED);
    }
}
