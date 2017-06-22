package org.ff4j.spring.boot.web.api.resources;

import static org.ff4j.services.constants.FeatureConstants.PARAM_GROUP;
import static org.ff4j.services.constants.FeatureConstants.PARAM_ROLE;
import static org.ff4j.services.constants.FeatureConstants.PATH_PARAM_GROUP;
import static org.ff4j.services.constants.FeatureConstants.PATH_PARAM_ROLE;
import static org.ff4j.services.constants.FeatureConstants.PATH_PARAM_UID;
import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J_STORE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ADDGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_DISABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ENABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_GRANTROLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEGROUP;
import static org.ff4j.web.FF4jWebConstants.OPERATION_REMOVEROLE;
import static org.ff4j.web.FF4jWebConstants.PARAM_UID;
import static org.springframework.http.HttpStatus.ACCEPTED;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.DELETE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;
import static org.springframework.web.bind.annotation.RequestMethod.PUT;

import org.ff4j.services.FeatureServices;
import org.ff4j.services.domain.FeatureApiBean;
import org.ff4j.spring.boot.web.api.utils.FeatureWebUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

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

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_FF4J_STORE_FEATURES + "/" + PATH_PARAM_UID)
public class FeatureResource {

    @Autowired
    private FeatureServices featureServices;


    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    public FeatureApiBean getFeatureByUID(@PathVariable(value = PARAM_UID) String featureUID) {
        return featureServices.getFeature(featureUID);
    }

    @RequestMapping(method = PUT, consumes = APPLICATION_JSON_VALUE, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Create or update a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 400, message = "Feature uid is blank (or) feature uid did not match with the requested feature uid to be created or updated"),
            @ApiResponse(code = 201, message = "Feature has been created"),
            @ApiResponse(code = 202, message = "Feature has been updated"),
            @ApiResponse(code = 204, message = "No content, no changes made to the feature")})
    public ResponseEntity<Boolean> createOrUpdateFeature(@PathVariable(value = PARAM_UID) String featureUID, @RequestBody FeatureApiBean featureApiBean) {
        return FeatureWebUtils.getBooleanResponseEntityByHttpStatus(featureServices.createOrUpdateFeature(featureUID, featureApiBean));
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(method = DELETE)
    @ApiOperation(value = "Delete a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 204, message = "No content, feature is deleted"),
            @ApiResponse(code = 404, message = "Feature not found")
    })
    public ResponseEntity<?> deleteFeature(@PathVariable(value = PARAM_UID) String featureUID) {
        featureServices.deleteFeature(featureUID);
        return new ResponseEntity(NO_CONTENT);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_ENABLE, method = POST)
    @ApiOperation(value = "Enable a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Features has been enabled"),
            @ApiResponse(code = 404, message = "Feature not found")})
    public ResponseEntity<?> enableFeature(@PathVariable(value = PARAM_UID) String featureUID) {
        featureServices.enableFeature(featureUID);
        return new ResponseEntity(ACCEPTED);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_DISABLE, method = POST)
    @ApiOperation(value = "Disable a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Features has been disabled"),
            @ApiResponse(code = 404, message = "Feature not found")})
    public ResponseEntity disableFeature(@PathVariable(value = PARAM_UID) String featureUID) {
        featureServices.disableFeature(featureUID);
        return new ResponseEntity(ACCEPTED);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_GRANTROLE + "/" + PATH_PARAM_ROLE, method = POST)
    @ApiOperation(value = "Grant a permission to a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Permission has been granted"),
            @ApiResponse(code = 404, message = "Feature not found"),
            @ApiResponse(code = 304, message = "Role already exists, nothing to update")})
    public ResponseEntity grantRoleToFeature(@PathVariable(value = PARAM_UID) String featureUID, @PathVariable(value = PARAM_ROLE) String role) {
        featureServices.grantRoleToFeature(featureUID, role);
        return new ResponseEntity(ACCEPTED);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_REMOVEROLE + "/" + PATH_PARAM_ROLE, method = POST)
    @ApiOperation(value = "Remove a permission from a feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Permission has been granted"),
            @ApiResponse(code = 404, message = "Feature not found")})
    public ResponseEntity removeRoleFromFeature(@PathVariable(value = PARAM_UID) String featureUID, @PathVariable(value = PARAM_ROLE) String role) {
        featureServices.removeRoleFromFeature(featureUID, role);
        return new ResponseEntity(ACCEPTED);
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + OPERATION_ADDGROUP + "/" + PATH_PARAM_GROUP, method = POST)
    @ApiOperation(value = "Define the group of the feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 202, message = "Group has been defined"),
            @ApiResponse(code = 404, message = "Feature not found"),
            @ApiResponse(code = 304, message = "Group already exists, nothing to update")})
    public ResponseEntity addGroupToFeature(@PathVariable(value = PARAM_UID) String featureUID, @PathVariable(value = PARAM_GROUP) String groupName) {
        featureServices.addGroupToFeature(featureUID, groupName);
        return new ResponseEntity(ACCEPTED);
    }

    @RequestMapping(value = "/" + OPERATION_REMOVEGROUP + "/" + PATH_PARAM_GROUP, method = POST)
    @ApiOperation(value = "Remove the group of the feature", response = ResponseEntity.class)
    @ApiResponses({
            @ApiResponse(code = 204, message = "Group has been removed"),
            @ApiResponse(code = 404, message = "Feature not found")})
    @SuppressWarnings("rawtypes")
    public ResponseEntity removeGroupFromFeature(@PathVariable(value = PARAM_UID) String featureUID, @PathVariable(value = PARAM_GROUP) String groupName) {
        featureServices.removeGroupFromFeature(featureUID, groupName);
        return new ResponseEntity(ACCEPTED);
    }
}
