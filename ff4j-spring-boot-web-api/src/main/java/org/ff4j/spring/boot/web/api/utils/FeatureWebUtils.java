package org.ff4j.spring.boot.web.api.utils;

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

import org.ff4j.services.model.FeatureActions;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;

import static java.lang.Boolean.FALSE;
import static java.lang.Boolean.TRUE;

/**
 * Created by Paul
 *
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public enum FeatureWebUtils {

    INSTANCE;

    public static ResponseEntity<Boolean> getBooleanResponseEntityByHttpStatus(FeatureActions featureActions) {
        switch (featureActions) {
            case CREATED:
                return new ResponseEntity<Boolean>(TRUE, HttpStatus.CREATED);
            case UPDATED:
                return new ResponseEntity<Boolean>(TRUE, HttpStatus.ACCEPTED);
            default:
                return new ResponseEntity<Boolean>(FALSE, HttpStatus.NO_CONTENT);
        }
    }
}
