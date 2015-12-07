package org.ff4j.web.api;

import java.lang.annotation.Annotation;
import java.util.Set;

import javax.inject.Inject;
import javax.ws.rs.core.Application;

import org.ff4j.FF4j;
import org.ff4j.web.FF4jApiConfig;
import org.ff4j.web.api.filter.JerseyApplicationEventListener;
import org.ff4j.web.api.filter.JerseyRequestEventListener;
import org.ff4j.web.api.resources.FF4jResource;
import org.glassfish.hk2.api.ServiceLocator;
import org.glassfish.hk2.utilities.binding.AbstractBinder;
import org.glassfish.jersey.message.filtering.EntityFilteringFeature;
import org.glassfish.jersey.message.filtering.SecurityAnnotations;
import org.glassfish.jersey.message.filtering.SecurityEntityFilteringFeature;
import org.glassfish.jersey.server.ResourceConfig;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.swagger.config.ScannerFactory;
import io.swagger.jaxrs.config.BeanConfig;

/**
 * Parent class to provide FF4J REST API using Jersey2x.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
public class FF4jApiApplicationJersey2 extends Application {

    /** logger for this class. */
    protected final Logger log = LoggerFactory.getLogger(getClass());

    private static final FF4j ff4j = null;

    private FF4jApiConfig apiConfig;

    public static class FF4jBinder extends AbstractBinder {
        @Override
        protected void configure() {
            // singleton instance binding
            bind(ff4j).to(FF4j.class);
        }
    }

    /**
     * Initialisation of Jersey2 application.
     *
     * @param serviceLocator
     */
    @Inject
    public FF4jApiApplicationJersey2(ServiceLocator serviceLocator) {
        log.info("  __  __ _  _   _ ");
        log.info(" / _|/ _| || | (_)");
        log.info("| |_| |_| || |_| |");
        log.info("|  _|  _|__   _| |");
        log.info("|_| |_|    |_|_/ |");
        log.info("             |__/   WEB API Initialization...");
        log.info(" ");

        ResourceConfig rc = new ResourceConfig();
        rc.packages(FF4jResource.class.getPackage().getName());

        rc.register(new FF4jBinder());
        rc.register(JerseyApplicationEventListener.class);
        rc.register(JerseyRequestEventListener.class);
        
        rc.property(EntityFilteringFeature.ENTITY_FILTERING_SCOPE,
                new Annotation[] {SecurityAnnotations.rolesAllowed("manager")});

        // Register the SecurityEntityFilteringFeature.
        rc.register(SecurityEntityFilteringFeature.class);
    
    
        // Swagger configuration
        BeanConfig beanConfig = new BeanConfig();
        beanConfig.setTitle("FF4J (ff4j.org) WebAPI");
        beanConfig.setDescription("Administrate and operate all tasks on your features through this api");
        beanConfig.setResourcePackage("org.ff4j.web.api.resources");
        beanConfig.setContact("@clunven");
        beanConfig.setLicense("Apache 2.0");
        beanConfig.setLicenseUrl("http://www.apache.org/licenses/LICENSE-2.0.html");
        beanConfig.setVersion(apiConfig.getVersionNumber());
        beanConfig.setSchemes(new String[] {"http"});
        beanConfig.setHost(apiConfig.getHost() + ":" + apiConfig.getPort());
        beanConfig.setBasePath("/" + apiConfig.getWebContext() + "/api");
        beanConfig.setScan(true);
        
        ScannerFactory.setScanner(beanConfig);
        getSingletons().add(io.swagger.jaxrs.listing.ApiListingResource.class);
        getSingletons().add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
    }

    /** {@inheritDoc} */
    @Override
    public Set<Class<?>> getClasses() {
        Set<Class<?>> classes = super.getClasses();
        classes.add(io.swagger.jaxrs.listing.ApiListingResource.class);
        classes.add(io.swagger.jaxrs.listing.SwaggerSerializers.class);
        return classes;
    }

}
