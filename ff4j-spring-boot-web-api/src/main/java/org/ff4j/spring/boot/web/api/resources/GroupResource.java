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
import org.ff4j.services.GroupServices;
import org.ff4j.services.domain.FeatureApiBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.Collection;

import static org.ff4j.services.constants.FeatureConstants.*;
import static org.ff4j.web.FF4jWebConstants.OPERATION_DISABLE;
import static org.ff4j.web.FF4jWebConstants.OPERATION_ENABLE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_FF4J_STORE_GROUPS + "/" + PATH_PARAM_GROUP)
public class GroupResource {

    @Autowired
    private GroupServices groupServices;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Get all the features belonging to the group", response = FeatureApiBean.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "features belonging to the group"),
            @ApiResponse(code = 404, message = "Group not found")})
    public Collection<FeatureApiBean> getFeaturesByGroup(@PathVariable(value = PARAM_GROUP) String groupName) {
        return groupServices.getFeaturesByGroup(groupName);
    }

    @RequestMapping(value = "/" + OPERATION_ENABLE, method = POST, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Enable a group", response = Void.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "Group has been enabled"),
            @ApiResponse(code = 404, message = "Group not found")})
    public void enableGroup(@PathVariable(value = PARAM_GROUP) String groupName) {
        groupServices.enableGroup(groupName);
    }

    @RequestMapping(value = "/" + OPERATION_DISABLE, method = POST, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Disable a group", response = Void.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "Group has been disabled"),
            @ApiResponse(code = 404, message = "Group not found")})
    public void disableGroup(@PathVariable(value = PARAM_GROUP) String groupName) {
        groupServices.disableGroup(groupName);
    }
}
