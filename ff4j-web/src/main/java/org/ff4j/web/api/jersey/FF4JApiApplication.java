package org.ff4j.web.api.jersey;

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

import javax.ws.rs.ApplicationPath;
import javax.ws.rs.core.Context;

import org.ff4j.FF4j;
import org.ff4j.web.api.FF4jWebConstants;
import org.ff4j.web.api.resources.FF4jResource;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jersey.api.container.filter.LoggingFilter;
import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.api.core.ResourceConfig;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;

/**
 * 
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 *
 */
@ApplicationPath("api")
public abstract class FF4JApiApplication extends PackagesResourceConfig implements FF4jWebConstants {

    /** logger for this class. */
    protected final Logger log = LoggerFactory.getLogger(getClass());

    /**
     * Child class must fullfil the configuration of the apicd
     * 
     * @return
     */
    protected abstract FF4jApiConfig getApiConfig();

    /**
     * Injection of bean ff4j within this static class.
     *
     * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
     */
    public static class FF4jInjectableProvider extends SingletonTypeInjectableProvider<Context, FF4j> {
        public FF4jInjectableProvider(FF4j ff4j) {
            super(FF4j.class, ff4j);
        }
    }

    /**
     * Constructor to defined resources.
     */
    public FF4JApiApplication() {
        super(FF4jResource.class.getPackage().getName());
        
        // Initialize through configuration
        FF4jApiConfig conf = getApiConfig();
        FF4jSecurityContextFilter.securityConfig = conf;
        FF4jRolesResourceFilterFactory.apiConfig = conf;
        
        // Register ff4J bean to be injected into resources.
        getSingletons().add(new FF4jInjectableProvider(conf.getFF4j()));
        
        // Authorization, JSR250
        if (conf.isEnableAuthorization()) {
            getProperties().put(ResourceConfig.PROPERTY_RESOURCE_FILTER_FACTORIES,
                    FF4jRolesResourceFilterFactory.class.getCanonicalName());
            log.info("ff4j webApi security has been set up with both authentication and authorization");

        } else if (conf.isEnableAuthentication()) {
            // Only Authenticated here
            StringBuilder filters = new StringBuilder();
            filters.append(FF4jSecurityContextFilter.class.getCanonicalName());
            if (conf.isEnableLogging()) {
                filters.append(";" + LoggingFilter.class.getCanonicalName());
            }
            // Pas authorization
            getProperties().put(ResourceConfig.PROPERTY_CONTAINER_REQUEST_FILTERS, filters.toString());
            log.info("ff4j webApi security has been set up with authentication only");

        } else {
            // No security
            log.info("ff4j webApi security has been set up with no seucrity");
        }
    }

}
