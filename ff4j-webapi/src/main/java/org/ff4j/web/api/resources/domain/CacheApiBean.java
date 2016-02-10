package org.ff4j.web.api.resources.domain;

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

import java.util.HashSet;
import java.util.Set;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FeatureStore;
import org.ff4j.property.store.PropertyStore;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Information of Cache.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@JsonInclude(Include.NON_NULL)
@ApiModel( value = "cacheApiBean", description = "cache resource representation" )
public class CacheApiBean {
    
    /** cacheProvider. */
    @JsonProperty("cacheProvider")
    @ApiModelProperty( value = "if a cachestore is defined", required = false )
    private String cacheProvider = null;
    
    /** cacheStore. */
    @JsonProperty("cacheStore")
    @ApiModelProperty( value = "if a cachestore is defined", required = false )
    private String cacheStore = null;
    
    @JsonProperty("featureNames")
    @ApiModelProperty( value = "list of features within cache", required = false )
    private Set < String > featureNames = new HashSet<String>();
    
    @JsonProperty("propertyNames")
    @ApiModelProperty( value = "list of properties within cache", required = false )
    private Set < String > propertyNames = new HashSet<String>();

    /**
     * Constructor from its feature store.
     *
     * @param featureStore
     *      cuurent fature store
     */
    public CacheApiBean(FeatureStore featureStore) {
        if (featureStore instanceof FF4jCacheProxy) {
            FF4jCacheProxy cacheProxy = (FF4jCacheProxy) featureStore;
            cacheStore    = cacheProxy.getCachedTargetStore();
            cacheProvider = cacheProxy.getCacheProvider();
            featureNames  = cacheProxy.getCacheManager().listCachedFeatureNames();
        }
    }
    
    /**
     * Constructor from its feature store.
     *
     * @param featureStore
     *      cuurent fature store
     */
    public CacheApiBean(PropertyStore featureStore) {
        if (featureStore instanceof FF4jCacheProxy) {
            FF4jCacheProxy cacheProxy = (FF4jCacheProxy) featureStore;
            cacheStore    = cacheProxy.getCachedTargetStore();
            cacheProvider = cacheProxy.getCacheProvider();
            propertyNames  = cacheProxy.getCacheManager().listCachedPropertyNames();
        }
    }
    
    /**
     * Getter accessor for attribute 'cacheProvider'.
     *
     * @return
     *       current value of 'cacheProvider'
     */
    public String getCacheProvider() {
        return cacheProvider;
    }

    /**
     * Setter accessor for attribute 'cacheProvider'.
     * @param cacheProvider
     * 		new value for 'cacheProvider '
     */
    public void setCacheProvider(String cacheProvider) {
        this.cacheProvider = cacheProvider;
    }

    /**
     * Getter accessor for attribute 'cacheStore'.
     *
     * @return
     *       current value of 'cacheStore'
     */
    public String getCacheStore() {
        return cacheStore;
    }

    /**
     * Setter accessor for attribute 'cacheStore'.
     * @param cacheStore
     * 		new value for 'cacheStore '
     */
    public void setCacheStore(String cacheStore) {
        this.cacheStore = cacheStore;
    }

    /**
     * Getter accessor for attribute 'featureNames'.
     *
     * @return
     *       current value of 'featureNames'
     */
    public Set<String> getFeatureNames() {
        return featureNames;
    }

    /**
     * Setter accessor for attribute 'featureNames'.
     * @param featureNames
     * 		new value for 'featureNames '
     */
    public void setFeatureNames(Set<String> featureNames) {
        this.featureNames = featureNames;
    }

    /**
     * Getter accessor for attribute 'propertyNames'.
     *
     * @return
     *       current value of 'propertyNames'
     */
    public Set<String> getPropertyNames() {
        return propertyNames;
    }

    /**
     * Setter accessor for attribute 'propertyNames'.
     * @param propertyNames
     * 		new value for 'propertyNames '
     */
    public void setPropertyNames(Set<String> propertyNames) {
        this.propertyNames = propertyNames;
    }

}
