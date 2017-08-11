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

import java.util.ArrayList;
import java.util.List;

import org.ff4j.cache.FF4jCacheProxy;
import org.ff4j.core.FeatureStore;

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Representation of the store.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "featureStoreApiBean", description = "store resource representation" )
public class FeatureStoreApiBean {
    
    /** type. */
    @JsonProperty("type")
    @ApiModelProperty( value = "classname of implementation", required = true )
    private String type;
    
    /** numberOfFeatures. */
    @JsonProperty("numberOfFeatures")
    @ApiModelProperty( value = "number of features", required = true )
    private int numberOfFeatures = 0;
    
    /** numberOfGroups. */
    @JsonProperty("numberOfGroups")
    @ApiModelProperty( value = "number of group", required = true )
    private int numberOfGroups = 0;

    /** features. */
    @JsonProperty("features")
    @ApiModelProperty( value = "list of features", required = true )
    private List < String > features = new ArrayList<String>();
    
    /** groups. */
    @JsonProperty("groups")
    @ApiModelProperty( value = "list of group", required = true )
    private List < String > groups = new ArrayList<String>();
    
    /** cached. */
    @JsonProperty("cache")
    @ApiModelProperty( value = "if a cachestore is defined", required = true )
    private CacheApiBean cache = null;
    
    /**
     * Default constructor.
     */
    public FeatureStoreApiBean() {
    }
            
    /**
     * Constructor from its feature store.
     *
     * @param featureStore
     *      cuurent fature store
     */
    public FeatureStoreApiBean(FeatureStore featureStore) {
        type = featureStore.getClass().getName();
        if (featureStore instanceof FF4jCacheProxy) {
            cache = new CacheApiBean(featureStore);
        }
        features = new ArrayList<String>(featureStore.readAll().keySet());
        groups   = new ArrayList<String>(featureStore.readAllGroups());
        numberOfFeatures = features.size();
        numberOfGroups = groups.size();
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
     * 		new value for 'type '
     */
    public void setType(String type) {
        this.type = type;
    }

    /**
     * Getter accessor for attribute 'numberOfFeatures'.
     *
     * @return
     *       current value of 'numberOfFeatures'
     */
    public int getNumberOfFeatures() {
        return numberOfFeatures;
    }

    /**
     * Setter accessor for attribute 'numberOfFeatures'.
     * @param numberOfFeatures
     * 		new value for 'numberOfFeatures '
     */
    public void setNumberOfFeatures(int numberOfFeatures) {
        this.numberOfFeatures = numberOfFeatures;
    }

    /**
     * Getter accessor for attribute 'numberOfGroups'.
     *
     * @return
     *       current value of 'numberOfGroups'
     */
    public int getNumberOfGroups() {
        return numberOfGroups;
    }

    /**
     * Setter accessor for attribute 'numberOfGroups'.
     * @param numberOfGroups
     * 		new value for 'numberOfGroups '
     */
    public void setNumberOfGroups(int numberOfGroups) {
        this.numberOfGroups = numberOfGroups;
    }

    /**
     * Getter accessor for attribute 'features'.
     *
     * @return
     *       current value of 'features'
     */
    public List<String> getFeatures() {
        return features;
    }

    /**
     * Setter accessor for attribute 'features'.
     * @param features
     * 		new value for 'features '
     */
    public void setFeatures(List<String> features) {
        this.features = features;
    }

    /**
     * Getter accessor for attribute 'groups'.
     *
     * @return
     *       current value of 'groups'
     */
    public List<String> getGroups() {
        return groups;
    }

    /**
     * Setter accessor for attribute 'groups'.
     * @param groups
     * 		new value for 'groups '
     */
    public void setGroups(List<String> groups) {
        this.groups = groups;
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
     * 		new value for 'cache '
     */
    public void setCache(CacheApiBean cache) {
        this.cache = cache;
    }

}
