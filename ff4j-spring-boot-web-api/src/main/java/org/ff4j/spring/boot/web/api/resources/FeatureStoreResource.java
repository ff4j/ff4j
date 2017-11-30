package org.ff4j.spring.boot.web.api.resources;

import static org.ff4j.services.constants.FeatureConstants.RESOURCE_CLEAR_CACHE;
import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J_STORE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_CACHE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_FEATURES;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_GROUPS;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;

import java.util.Collection;

import org.ff4j.services.FeatureStoreServices;
import org.ff4j.services.domain.CacheApiBean;
import org.ff4j.services.domain.FeatureApiBean;
import org.ff4j.services.domain.FeatureStoreApiBean;
import org.ff4j.services.domain.GroupDescApiBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
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
@RequestMapping(value = RESOURCE_FF4J_STORE)
public class FeatureStoreResource {
    
    @Autowired
    private FeatureStoreServices featureStoreService;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(
            value = "Displays information regarding the <b>FeaturesStore</b>",
            response = FeatureStoreApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message = "status of current feature store", response = FeatureStoreApiBean.class))
    public FeatureStoreApiBean getFeatureStore() {
        return featureStoreService.getFeatureStore();
    }

    @RequestMapping(value = "/" + RESOURCE_FEATURES, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Displays all the <b>Features</b>", response = FeatureApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message = "get all features"))
    public Collection<FeatureApiBean> getAllFeatures() {
        return featureStoreService.getAllFeatures();
    }

    @RequestMapping(value = "/" + RESOURCE_GROUPS, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Display information regarding <b>Groups</b>", response = GroupDescApiBean.class)
    @ApiResponses({@ApiResponse(code = 200, message = "Groups resource", response = GroupDescApiBean.class)})
    public Collection<GroupDescApiBean> getAllGroups() {
        return featureStoreService.getAllGroups();
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + STORE_CLEAR, method = RequestMethod.DELETE)
    @ApiOperation(value = "Delete all <b>Features</b> in store")
    @ApiResponses(@ApiResponse(code = 204, message = "all feature have been deleted"))
    public ResponseEntity deleteAllFeatures() {
        featureStoreService.deleteAllFeatures();
        return new ResponseEntity(NO_CONTENT);
    }

    @RequestMapping(value = "/" + RESOURCE_CACHE, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Display information related to <b>Cache</b>")
    @ApiResponses({@ApiResponse(code = 200, message = "Gets the cached features", response = CacheApiBean.class),
            @ApiResponse(code = 404, message = "feature store is not cached")})
    public CacheApiBean getFeaturesFromCache() {
        return featureStoreService.getFeaturesFromCache();
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + RESOURCE_CLEAR_CACHE, method = RequestMethod.DELETE)
    @ApiOperation(value = "Clear cache", response = ResponseEntity.class)
    @ApiResponses({@ApiResponse(code = 204, message = "cache is cleared"),
            @ApiResponse(code = 404, message = "feature store is not cached")})
    public ResponseEntity clearCachedFeatureStore() {
        featureStoreService.clearCachedFeatureStore();
        return new ResponseEntity(NO_CONTENT);
    }
}
