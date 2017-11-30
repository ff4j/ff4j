package org.ff4j.spring.boot.web.api.exceptions;

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

import org.ff4j.exception.InvalidPropertyTypeException;
import org.ff4j.services.exceptions.AuthorizationNotExistsException;
import org.ff4j.services.exceptions.FeatureIdBlankException;
import org.ff4j.services.exceptions.FeatureIdNotMatchException;
import org.ff4j.services.exceptions.FeatureNotFoundException;
import org.ff4j.services.exceptions.FeatureStoreNotCached;
import org.ff4j.services.exceptions.FlippingStrategyBadRequestException;
import org.ff4j.services.exceptions.GroupExistsException;
import org.ff4j.services.exceptions.GroupNotExistsException;
import org.ff4j.services.exceptions.PropertiesBadRequestException;
import org.ff4j.services.exceptions.PropertyNameBlankException;
import org.ff4j.services.exceptions.PropertyNameNotMatchException;
import org.ff4j.services.exceptions.PropertyNotFoundException;
import org.ff4j.services.exceptions.PropertyStoreNotCached;
import org.ff4j.services.exceptions.RoleExistsException;
import org.ff4j.services.exceptions.RoleNotExistsException;
import org.springframework.core.annotation.Order;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;
import org.springframework.web.bind.annotation.ResponseStatus;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@ControllerAdvice(basePackages = {"org.ff4j.spring.boot.web.api.resources"})
@Order(0)
public class FF4jExceptionHandler {
    
    @ExceptionHandler(value = IllegalArgumentException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "bad request")
    public void badRequestHandler() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = FeatureNotFoundException.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "feature not found")
    public void featureNotFoundException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = FeatureIdBlankException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "feature uid cannot be blank")
    public void featureIdBlankException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = FeatureIdNotMatchException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "feature uid did not match with the requested feature uid to be created or updated")
    public void featureIdNotMatchException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = FlippingStrategyBadRequestException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "flipping strategy specified wrongly")
    public void flippingStrategyBadRequestException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = PropertiesBadRequestException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "properties specified wrongly")
    public void propertiesBadRequestException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = RoleExistsException.class)
    @ResponseStatus(value = HttpStatus.NOT_MODIFIED, reason = "role already exists")
    public void roleExistsException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = RoleNotExistsException.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "role does not exist")
    public void roleNotExistsException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = GroupExistsException.class)
    @ResponseStatus(value = HttpStatus.NOT_MODIFIED, reason = "group already exists")
    public void groupExistsException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = GroupNotExistsException.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "group does not exist")
    public void groupNotExistsException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = FeatureStoreNotCached.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "feature store is not cached")
    public void featureStoreNotCached() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = AuthorizationNotExistsException.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "no security has been defined")
    public void authorizationNotExistsException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = PropertyNotFoundException.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "property not found")
    public void propertyNotFoundException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = InvalidPropertyTypeException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "bad request")
    public void propertyValueInvalidException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = PropertyNameBlankException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "property name cannot be blank")
    public void propertyNameBlankException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = PropertyNameNotMatchException.class)
    @ResponseStatus(value = HttpStatus.BAD_REQUEST, reason = "property name did not match with the requested property name to be created or updated")
    public void propertyNameNotMatchException() {
        // Not necessary to handle this exception
    }

    @ExceptionHandler(value = PropertyStoreNotCached.class)
    @ResponseStatus(value = HttpStatus.NOT_FOUND, reason = "property store is not cached")
    public void propertyStoreNotCached() {
        // Not necessary to handle this exception
    }
}
