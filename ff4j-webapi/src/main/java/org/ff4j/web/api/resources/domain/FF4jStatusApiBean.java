package org.ff4j.web.api.resources.domain;

/*
 * #%L
 * ff4j-webapi
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


import org.ff4j.FF4j;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * API Bean to represent ff4j.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel(value = "ff4jStatus", description = "ff4j resource representation" )
@JsonInclude(Include.NON_NULL)
public final class FF4jStatusApiBean {
    
    /** uptime. */
    @ApiModelProperty( value = "uptime of the application", required = true )
    @JsonProperty("uptime")
    private String uptime;
    
    /** autocreate. */
    @ApiModelProperty( value = "feature are created if not exist in store", required = true )
    @JsonProperty("autocreate")
    private boolean autocreate = false;
    
    /** version. */
    @ApiModelProperty( value = "current version of ff4j library", required = true )
    @JsonProperty("version")
    private String version = "N/A";
    
    /** store. */
    @ApiModelProperty( value = "feature store resource representation", required = true )
    @JsonProperty(value = "featuresStore")
    private FeatureStoreApiBean featuresStore = null;
    
    /** event repository. */
    @ApiModelProperty( value = "event repository resource representation", required = false )
    @JsonProperty("eventRepository")
    private EventRepositoryApiBean eventRepository = null;
    
    /** authorization manager. */
    @ApiModelProperty( value = "authorization manager resource representation", required = false )
    @JsonProperty("authorizationsManager")
    private AuthorizationsManagerApiBean authorizationsManager = null;
    
    /**
     * Default constructor.
     *
     * @param ff4j
     *      target ff4j.
     */
    public FF4jStatusApiBean() {
    }
            
    /**
     * Parameterized Constructor.
     *
     * @param ff4j
     *      target ff4j.
     */
    public FF4jStatusApiBean(FF4j ff4j) {
        // UpTime
        long up = System.currentTimeMillis() - ff4j.getStartTime();
        long daynumber = up / (1000 * 3600 * 24);
        up = up - (daynumber * 1000 * 3600 * 24);
        long hourNumber = up / (1000 * 3600);
        up = up - (hourNumber * 1000 * 3600);
        long minutenumber = up / (1000 * 60);
        up = up - (minutenumber * 1000 * 60);
        long secondnumber = up / 1000;
        uptime =  daynumber + " day(s) ";
        uptime += hourNumber + " hours(s) ";
        uptime += minutenumber + " minute(s) ";
        uptime += secondnumber + " seconds\"";
        autocreate = ff4j.isAutocreate();
        version = ff4j.getVersion();
        if (null != ff4j.getFeatureStore()) {
            featuresStore = new FeatureStoreApiBean(ff4j.getFeatureStore());
        }
        if (null != ff4j.getEventRepository()) {
            eventRepository = new EventRepositoryApiBean(ff4j.getEventRepository(), null, null);
        }
        if (null != ff4j.getAuthorizationsManager()) {
            authorizationsManager = new AuthorizationsManagerApiBean(ff4j.getAuthorizationsManager());
        }
    }

    /**
     * Getter accessor for attribute 'uptime'.
     *
     * @return
     *       current value of 'uptime'
     */
    public String getUptime() {
        return uptime;
    }

    /**
     * Setter accessor for attribute 'uptime'.
     * @param uptime
     * 		new value for 'uptime '
     */
    public void setUptime(String uptime) {
        this.uptime = uptime;
    }

    /**
     * Getter accessor for attribute 'autocreate'.
     *
     * @return
     *       current value of 'autocreate'
     */
    public boolean isAutocreate() {
        return autocreate;
    }

    /**
     * Setter accessor for attribute 'autocreate'.
     * @param autocreate
     * 		new value for 'autocreate '
     */
    public void setAutocreate(boolean autocreate) {
        this.autocreate = autocreate;
    }

    /**
     * Getter accessor for attribute 'version'.
     *
     * @return
     *       current value of 'version'
     */
    public String getVersion() {
        return version;
    }

    /**
     * Setter accessor for attribute 'version'.
     * @param version
     * 		new value for 'version '
     */
    public void setVersion(String version) {
        this.version = version;
    }

    /**
     * Getter accessor for attribute 'featuresStore'.
     *
     * @return
     *       current value of 'featuresStore'
     */
    public FeatureStoreApiBean getFeaturesStore() {
        return featuresStore;
    }

    /**
     * Setter accessor for attribute 'featuresStore'.
     * @param featuresStore
     * 		new value for 'featuresStore '
     */
    public void setFeaturesStore(FeatureStoreApiBean featuresStore) {
        this.featuresStore = featuresStore;
    }

    /**
     * Getter accessor for attribute 'eventRepository'.
     *
     * @return
     *       current value of 'eventRepository'
     */
    public EventRepositoryApiBean getEventRepository() {
        return eventRepository;
    }

    /**
     * Setter accessor for attribute 'eventRepository'.
     * @param eventRepository
     * 		new value for 'eventRepository '
     */
    public void setEventRepository(EventRepositoryApiBean eventRepository) {
        this.eventRepository = eventRepository;
    }

    /**
     * Getter accessor for attribute 'authorizationsManager'.
     *
     * @return
     *       current value of 'authorizationsManager'
     */
    public AuthorizationsManagerApiBean getAuthorizationsManager() {
        return authorizationsManager;
    }

    /**
     * Setter accessor for attribute 'authorizationsManager'.
     * @param authorizationsManager
     * 		new value for 'authorizationsManager '
     */
    public void setAuthorizationsManager(AuthorizationsManagerApiBean authorizationsManager) {
        this.authorizationsManager = authorizationsManager;
    }

}
