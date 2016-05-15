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

import org.apache.commons.lang3.StringUtils;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public enum HttpMethod {

    GET("GET"), PUT("PUT"), POST("POST"), DELETE("DELETE");

    private String httpMethod;

    HttpMethod(String httpMethod) {
        this.httpMethod = httpMethod;
    }

    @Override
    public String toString() {
        return httpMethod;
    }

    public static org.springframework.http.HttpMethod getHttpMethod(String httpMethodString) {
        switch (getEnum(httpMethodString)) {
            case GET:
                return org.springframework.http.HttpMethod.GET;
            case PUT:
                return org.springframework.http.HttpMethod.PUT;
            case POST:
                return org.springframework.http.HttpMethod.POST;
            case DELETE:
                return org.springframework.http.HttpMethod.DELETE;
            default:
                throw new IllegalArgumentException();
        }
    }

    private static HttpMethod getEnum(String httpMethodString) {
        if (StringUtils.isBlank(httpMethodString)) {
            throw new IllegalArgumentException();
        }
        for (HttpMethod httpMethod : values()) {
            if (httpMethod.toString().equalsIgnoreCase(httpMethodString)) {
                return httpMethod;
            }
        }
        throw new IllegalArgumentException();
    }
}
