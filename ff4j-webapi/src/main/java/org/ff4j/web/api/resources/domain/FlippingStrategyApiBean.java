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

import java.util.HashMap;
import java.util.Map;

import org.ff4j.core.FlippingStrategy;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;
import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Bean for flipping strategy
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel(value = "flippingStrategyApiBean", description = "ff4j resource representation" )
@JsonInclude(Include.NON_NULL)
public class FlippingStrategyApiBean {
    
    /** Implementation class. */
    @ApiModelProperty( value = "implementation class", required = true )
    @JsonProperty("type")
    private String type = null;
    
    /**
     * Init params.
     */
    @ApiModelProperty( value = "init parameters", required = false )
    @JsonProperty("initParams")
    private Map < String, String > initParams = new HashMap<String, String>();
    
    
    /**
     * Target init parameters.
     *
     * @param fs
     *      current flipping strategy
     */
    public FlippingStrategyApiBean() {
    }
    
    /**
     * Target init parameters.
     *
     * @param fs
     *      current flipping strategy
     */
    public FlippingStrategyApiBean(FlippingStrategy fs) {
        this.type = fs.getClass().getName();
        this.initParams = fs.getInitParams();
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
     * Getter accessor for attribute 'initParams'.
     *
     * @return
     *       current value of 'initParams'
     */
    public Map<String, String> getInitParams() {
        return initParams;
    }

    /**
     * Setter accessor for attribute 'initParams'.
     * @param initParams
     * 		new value for 'initParams '
     */
    public void setInitParams(Map<String, String> initParams) {
        this.initParams = initParams;
    }

}
