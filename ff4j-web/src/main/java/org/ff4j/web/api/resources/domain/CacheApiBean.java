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

import org.codehaus.jackson.annotate.JsonProperty;
import org.ff4j.core.FeatureStore;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.wordnik.swagger.annotations.ApiModel;
import com.wordnik.swagger.annotations.ApiModelProperty;

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

    /**
     * Constructor from its feature store.
     *
     * @param featureStore
     *      cuurent fature store
     */
    public CacheApiBean(FeatureStore featureStore) {
        if (featureStore.isCached()) {
            cacheStore = featureStore.getCachedTargetStore();
            cacheProvider = featureStore.getCacheProvider();
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

}
