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
import org.ff4j.services.PropertyStoreServices;
import org.ff4j.services.constants.FeatureConstants;
import org.ff4j.services.domain.CacheApiBean;
import org.ff4j.services.domain.PropertyApiBean;
import org.ff4j.services.domain.PropertyStoreApiBean;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import java.util.List;

import static org.ff4j.services.constants.FeatureConstants.RESOURCE_FF4J_PROPERTY_STORE;
import static org.ff4j.web.FF4jWebConstants.RESOURCE_CACHE;
import static org.ff4j.web.FF4jWebConstants.STORE_CLEAR;
import static org.springframework.http.HttpStatus.NO_CONTENT;
import static org.springframework.http.MediaType.APPLICATION_JSON_VALUE;
import static org.springframework.web.bind.annotation.RequestMethod.DELETE;
import static org.springframework.web.bind.annotation.RequestMethod.GET;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@RestController
@RequestMapping(value = RESOURCE_FF4J_PROPERTY_STORE)
public class PropertyStoreResource {

    @Autowired
    private PropertyStoreServices propertyStoreServices;

    @RequestMapping(method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Display information regarding <b>Properties Store</b>", response = PropertyStoreApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message = "status of current properties store"))
    public PropertyStoreApiBean getPropertyStore() {
        return propertyStoreServices.getPropertyStore();
    }

    @RequestMapping(value = FeatureConstants.RESOURCE_PROPERTIES, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Display all the <b>Properties</b>", response = PropertyApiBean.class)
    @ApiResponses(@ApiResponse(code = 200, message = "get all Properties"))
    public List<PropertyApiBean> getAllProperties() {
        return propertyStoreServices.getAllProperties();
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = "/" + STORE_CLEAR, method = DELETE)
    @ApiOperation(value = "Delete all <b>Properties</b> in store")
    @ApiResponses(@ApiResponse(code = 204, message = "all properties have been deleted", response = ResponseEntity.class))
    public ResponseEntity deleteAllProperties() {
        propertyStoreServices.deleteAllProperties();
        return new ResponseEntity(NO_CONTENT);
    }

    @RequestMapping(value = "/" + RESOURCE_CACHE, method = GET, produces = APPLICATION_JSON_VALUE)
    @ApiOperation(value = "Display information related to <b>Cache</b>")
    @ApiResponses({@ApiResponse(code = 200, message = "Gets the cached properties", response = CacheApiBean.class),
            @ApiResponse(code = 404, message = "property store is not cached")})
    public CacheApiBean getPropertiesFromCache() {
        return propertyStoreServices.getPropertiesFromCache();
    }

    @SuppressWarnings("rawtypes")
    @RequestMapping(value = FeatureConstants.RESOURCE_CLEAR_CACHE, method = DELETE)
    @ApiOperation(value = "Clear cache", response = ResponseEntity.class)
    @ApiResponses({@ApiResponse(code = 204, message = "cache is cleared"),
            @ApiResponse(code = 404, message = "property store is not cached")})
    public ResponseEntity clearCachedPropertyStore() {
        propertyStoreServices.clearCachedPropertyStore();
        return new ResponseEntity(NO_CONTENT);
    }
}
