package org.ff4j.web.api.security;

/*
 * #%L
 * ff4j-web
 * %%
 * Copyright (C) 2013 - 2014 Ff4J
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

import java.util.ArrayList;
import java.util.List;

import org.ff4j.web.ApiConfig;

import com.sun.jersey.api.container.filter.RolesAllowedResourceFilterFactory;
import com.sun.jersey.api.model.AbstractMethod;
import com.sun.jersey.spi.container.ResourceFilter;

/**
 * List filters for the inbound request
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jRolesResourceFilterFactory extends RolesAllowedResourceFilterFactory {

    /** Api Config to initialize filters. */
    private static ApiConfig apiConfig = null;

    /** {@inheritDoc} */
    @Override
    public List<ResourceFilter> create(AbstractMethod am) {        
        
        // Enable authorization through RolesAllowed and JSR250 annotation
        List<ResourceFilter> rolesFilters = super.create(am);
        if (null == rolesFilters) {
            rolesFilters = new ArrayList<ResourceFilter>();
        }

        // Convert ionto mutable List, so as to add more filters that we need
        // (RolesAllowedResourceFilterFactory generates immutable list of filters)
        List<ResourceFilter> filters = new ArrayList<ResourceFilter>(rolesFilters);
        
        // As Authorization is enable, authenticate is mandator and came first
        filters.add(0, new FF4jSecurityContextFilter());

        return filters;
    }

    public static ApiConfig getApiConfig() {
        return apiConfig;
    }

    public static void setApiConfig(ApiConfig apiConfig) {
        FF4jRolesResourceFilterFactory.apiConfig = apiConfig;
    }
}
