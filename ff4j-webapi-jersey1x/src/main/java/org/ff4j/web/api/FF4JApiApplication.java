package org.ff4j.web.api;

/*
 * #%L
 * ff4j-webapi-jersey1x
 * %%
 * Copyright (C) 2013 - 2015 FF4J
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


import java.util.Set;

import javax.ws.rs.core.Application;
import javax.ws.rs.core.Context;

import org.codehaus.jackson.jaxrs.JacksonJsonProvider;
import org.ff4j.FF4j;
import org.ff4j.web.ApiConfig;
import org.ff4j.web.api.resources.FF4jResource;
import org.ff4j.web.api.security.FF4jRolesResourceFilterFactory;
import org.ff4j.web.api.security.FF4jSecurityContextFilter;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.sun.jersey.api.container.filter.LoggingFilter;
import com.sun.jersey.api.core.PackagesResourceConfig;
import com.sun.jersey.api.core.ResourceConfig;
import com.sun.jersey.api.json.JSONConfiguration;
import com.sun.jersey.spi.inject.SingletonTypeInjectableProvider;

import io.swagger.jaxrs.config.BeanConfig;


//@ApplicationPath("api")
/**
 * Jersey1x implementation of {@link Application} to create the webapi.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public abstract class FF4JApiApplication extends PackagesResourceConfig {

    /** logger for this class. */
    protected final Logger log = LoggerFactory.getLogger(getClass());

    /**
     * Constructor to defined resources.
     */
    public FF4JApiApplication() {
        super(FF4jResource.class.getPackage().getName());
        log.info("  __  __ _  _   _ ");
        log.info(" / _|/ _| || | (_)");
        log.info("| |_| |_| || |_| |");
        log.info("|  _|  _|__   _| |");
        log.info("|_| |_|    |_|_/ |");
        log.info("             |__/   WEB API Initialization...");
        log.info(" ");
        
        // Initialize through configuration
        ApiConfig conf = getApiConfig();
        FF4jSecurityContextFilter.setSecurityConfig(conf);
        FF4jRolesResourceFilterFactory.setApiConfig(conf);

        // Register ff4J bean to be injected into resources.
        getSingletons().add(new FF4jInjectableProvider(conf.getFF4j()));
        
        // Pojo Mapping to 'ON'
        getFeatures().put(JSONConfiguration.FEATURE_POJO_MAPPING, Boolean.TRUE);
        
        // Mapping Jackson Custom
        getSingletons().add(new JacksonJsonProvider());
        getSingletons().add(new FF4jJacksonMapper());
        
        // Authorization, JSR250
        if (conf.isAutorize()) {
            getProperties().put(ResourceConfig.PROPERTY_RESOURCE_FILTER_FACTORIES,
                    FF4jRolesResourceFilterFactory.class.getCanonicalName());
            log.info("ff4j webApi security has been set up with both authentication and authorization");

        } else if (conf.isAuthenticate()) {
            // Only Authenticated here
            StringBuilder filters = new StringBuilder();
            filters.append(FF4jSecurityContextFilter.class.getCanonicalName());
            if (conf.isLog()) {
                filters.append(";" + LoggingFilter.class.getCanonicalName());
            }
            // Pas authorization
            getProperties().put(ResourceConfig.PROPERTY_CONTAINER_REQUEST_FILTERS, filters.toString());
            log.info("ff4j webApi security has been set up with authentication only");

        } else {
            // No security
            log.info("ff4j webApi security has been set up with no security");
        }
        
        // Enable Documentation if required
        if (conf.isDocumentation()) {
           
           BeanConfig beanConfig = FF4jSwaggerConfiguration.getBeanConfig();
           beanConfig.setVersion(conf.getVersion());
           beanConfig.setSchemes(new String[] {"http"});
           beanConfig.setHost(conf.getHost() + ":" + conf.getPort());
           beanConfig.setBasePath("/" + conf.getWebContext() + "/api");
           beanConfig.setScan(true);
            
           getSingletons().add(io.swagger.jaxrs.listing.ApiListingResource.class);
           getSingletons().add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
        }
    }

    /**
     * Child class must fullfil the configuration of the apicd
     * 
     * @return
     */
    protected abstract ApiConfig getApiConfig();

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

    /** {@inheritDoc} */
    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> classes =  super.getClasses();
        classes.add(io.swagger.jaxrs.listing.ApiListingResource.class);
        classes.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
        return classes;
    }
    

}
