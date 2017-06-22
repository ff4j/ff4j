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
import org.ff4j.services.FF4jServices;
import org.ff4j.services.domain.AuthorizationsManagerApiBean;
import org.ff4j.services.domain.FF4jStatusApiBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.util.MultiValueMap;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import java.util.Map;

import static org.ff4j.services.constants.FeatureConstants.*;
import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J;
import static org.ff4j.web.FF4jWebConstants.*;
import static org.springframework.http.HttpStatus.OK;
import static org.springframework.http.MediaType.APPLICATION_FORM_URLENCODED_VALUE;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;
import static org.springframework.web.bind.annotation.RequestMethod.POST;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_FF4J)
public class FF4jResource {
    @Autowired
    private FF4jServices ff4JServices;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(
            value = "Gets <b>ff4j</b> status overview",
            notes = "Gets information related to <b>Monitoring</b>, <b>Security</b>, <b>Cache</b> and <b>Store</b>")
    @ApiResponses(
            @ApiResponse(code = 200, message = "Success, return status of ff4j instance", response = FF4jStatusApiBean.class))
    public FF4jStatusApiBean getStatus() {
        return ff4JServices.getStatus();
    }

    @RequestMapping(value = "/" + RESOURCE_SECURITY, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Gets <b>Security</b> information (permissions manager)",
            notes = "Security is implemented through dedicated <b>AuthorizationsManager</b> but it's not mandatory")
    @ApiResponses({@ApiResponse(code = 200, message = "Status of current ff4j security bean", response = AuthorizationsManagerApiBean.class),
            @ApiResponse(code = 404, message = "no security has been defined")})
    public AuthorizationsManagerApiBean getSecurityInfo() {
        return ff4JServices.getSecurityInfo();
    }


    @RequestMapping(value = "/" + OPERATION_CHECK + "/" + PATH_PARAM_UID, method = GET)
    @ApiOperation(value = "<b>Simple check</b> feature toggle", response = Boolean.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "If feature is flipped"),
            @ApiResponse(code = 404, message = "Feature not found")})
    public ResponseEntity<Boolean> check(@PathVariable(value = PARAM_UID) String featureUID) {
        Boolean status = ff4JServices.check(featureUID);
        return new ResponseEntity<Boolean>(status, OK);
    }

    @RequestMapping(value = "/" + OPERATION_CHECK + "/" + PATH_PARAM_UID, method = POST, consumes = APPLICATION_FORM_URLENCODED_VALUE)
    @ApiOperation(value = "<b>Advanced check</b> feature toggle (parametrized)", response = Boolean.class)
    @ApiResponses({
            @ApiResponse(code = 200, message = "If feature is flipped"),
            @ApiResponse(code = 400, message = "Invalid parameter"),
            @ApiResponse(code = 404, message = "Feature not found")})
    public ResponseEntity<Boolean> check(@PathVariable(value = PARAM_UID) String featureUID, @RequestParam MultiValueMap<String, String> formParams) {
        Map<String, String> map = formParams.toSingleValueMap();
        Boolean status = ff4JServices.check(featureUID, map);
        return new ResponseEntity<Boolean>(status, OK);
    }
}
