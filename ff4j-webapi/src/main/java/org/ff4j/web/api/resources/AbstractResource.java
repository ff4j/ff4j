package org.ff4j.web.api.resources;

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

import javax.ws.rs.core.Context;
import javax.ws.rs.core.Request;
import javax.ws.rs.core.SecurityContext;
import javax.ws.rs.core.UriInfo;

import org.ff4j.FF4j;
import org.ff4j.audit.repository.EventRepository;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.PropertyStore;

/**
 * SuperClass for common injections.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public abstract class AbstractResource {
    
    /** Access to Features through store. */
    @Context
    protected FF4j ff4j = null;

    /** rest url. */
    @Context
    protected UriInfo uriInfo;

    /** current request. */
    @Context
    protected Request request;
    
    /** security context is included within resources to get permissions. */
    @Context
    protected SecurityContext securityContext;
    
    /** Access to Features through store. */
    private FeatureStore store;
    
    /** Access to Features through store. */
    private PropertyStore propertyStore;
    
    /** Access to event repository. */
    private EventRepository repo;
     
    /**
     * Getter accessor for attribute 'repo'.
     *
     * @return
     *       current value of 'repo'
     */
    public EventRepository getRepo() {
        if (repo == null) {
            repo = ff4j.getEventRepository();
        }
        return repo;
    }
    
    /**
     * Getter accessor for attribute 'store'.
     * 
     * @return current value of 'store'
     */
    public FeatureStore getFeatureStore() {
        if (store == null) {
            store = ff4j.getFeatureStore();
        }
        return store;
    }

    /**
     * Getter accessor for attribute 'propertyStore'.
     *
     * @return
     *       current value of 'propertyStore'
     */
    public PropertyStore getPropertyStore() {
        if (propertyStore == null) {
            propertyStore = ff4j.getPropertiesStore();
        }
        return propertyStore;
    }

    /**
     * Setter accessor for attribute 'propertyStore'.
     * @param propertyStore
     * 		new value for 'propertyStore '
     */
    public void setPropertyStore(PropertyStore propertyStore) {
        this.propertyStore = propertyStore;
    }

}
