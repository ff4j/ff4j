package org.ff4j.web.api.resources.domain;

/*
 * #%L
 * ff4j-webapi
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


import java.util.HashSet;
import java.util.Set;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.property.store.PropertyStore;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * List available Properties
 * @author Cedrick Lunven (@clunven)</a>
 */
@ApiModel( value = "fpropertyStoreApiBean", description = "property store resource representation" )
public class PropertyStoreApiBean {

    /** type. */
    @JsonProperty("type")
    @ApiModelProperty( value = "classname of implementation", required = true )
    private String type;
    
    /** numberOfFeatures. */
    @JsonProperty("numberOfProperties")
    @ApiModelProperty( value = "number of properties", required = true )
    private int numberOfProperties = 0;
  
    /** features. */
    @JsonProperty("properties")
    @ApiModelProperty( value = "list of properties", required = true )
    private Set < String > properties = new HashSet<String>();
  
    /** cached. */
    @JsonProperty("cache")
    @ApiModelProperty( value = "if a cachestore is defined", required = true )
    private CacheApiBean cache = null;
    
    /**
     * Default constructor.
     */
    public PropertyStoreApiBean() {
    }
            
    /**
     * Constructor from its feature store.
     *
     * @param featureStore
     *      cuurent fature store
     */
    public PropertyStoreApiBean(PropertyStore pStore) {
        type = pStore.getClass().getName();
        if (pStore instanceof FF4jCacheProxy) {
            cache = new CacheApiBean(pStore);
        }
        properties = pStore.listPropertyNames();
        numberOfProperties = properties.size();
    }

    /**
     * Getter accessor for attribute 'type'.
     *
     * @return
     *       current value of 'type'
     */
    public String getType() {
        return type;
    }

    /**
     * Setter accessor for attribute 'type'.
     * @param type
     *      new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
    }
    
    /**
     * Getter accessor for attribute 'cache'.
     *
     * @return
     *       current value of 'cache'
     */
    public CacheApiBean getCache() {
        return cache;
    }

    /**
     * Setter accessor for attribute 'cache'.
     * @param cache
     *      new value for 'cache '
     */
    public void setCache(CacheApiBean cache) {
        this.cache = cache;
    }

    /**
     * Getter accessor for attribute 'numberOfProperties'.
     *
     * @return
     *       current value of 'numberOfProperties'
     */
    public int getNumberOfProperties() {
        return numberOfProperties;
    }

    /**
     * Setter accessor for attribute 'numberOfProperties'.
     * @param numberOfProperties
     * 		new value for 'numberOfProperties '
     */
    public void setNumberOfProperties(int numberOfProperties) {
        this.numberOfProperties = numberOfProperties;
    }

    /**
     * Getter accessor for attribute 'properties'.
     *
     * @return
     *       current value of 'properties'
     */
    public Set<String> getProperties() {
        return properties;
    }

    /**
     * Setter accessor for attribute 'properties'.
     * @param properties
     * 		new value for 'properties '
     */
    public void setProperties(Set<String> properties) {
        this.properties = properties;
    }

    

}
