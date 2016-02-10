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
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.ff4j.core.Feature;
import org.ff4j.property.Property;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Representation of a feature within Web API.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "featureApiBean", description = "Representation of a feature" )
@JsonInclude(Include.NON_NULL)
public class FeatureApiBean {
    
    /** unique feature identifier. */
    @JsonProperty("uid")
    @ApiModelProperty( value = "unique feature identifier", required = true )
    private String uid = null;
    
    /** status of feature. */
    @JsonProperty("enable")
    @ApiModelProperty( value = "status of feature", required = true )
    private boolean enable = false;
    
    /** status of feature. */
    @JsonProperty("description")
    @ApiModelProperty( value = "description of feature", required = false )
    private String description = null;
    
    /** status of feature. */
    @JsonProperty("group")
    @ApiModelProperty( value = "Group of the feature if exists, it's single", required = false )
    private String group = null;
    
    /** status of feature. */
    @JsonProperty("permissions")
    @ApiModelProperty( value = "Role and profiles authorized on feature", required = false )
    private List < String > permissions = new ArrayList<String>();
    
    /** status of feature. */
    @JsonProperty("flippingStrategy")
    @ApiModelProperty( value = "Flipping strategy if exist", required = false )
    private FlippingStrategyApiBean flippingStrategy = null;
    
    @JsonProperty("customProperties")
    @ApiModelProperty( value = "Custom properties if they exist", required = false )
    private Map < String, PropertyApiBean > customProperties = new HashMap< String, PropertyApiBean >();
    
    /**
     * Default Constructor.
     */
    public FeatureApiBean() {
    }
  
    /**
     * Copy constructor.
     *
     * @param f
     * 
     *  target feature
     */
    public FeatureApiBean(Feature f) {
        this.uid         = f.getUid();
        this.enable      = f.isEnable();
        this.description = f.getDescription();
        this.group       = f.getGroup();
        this.permissions = new ArrayList<String>(f.getPermissions());
        if (f.getFlippingStrategy() != null) {
            this.flippingStrategy = new FlippingStrategyApiBean(f.getFlippingStrategy());
        }
        if (f.getCustomProperties() != null) {
            for (Property<?> ap1 : f.getCustomProperties().values()) {
                customProperties.put(ap1.getName(), new PropertyApiBean(ap1));
            }
        }
    }

    /**
     * Getter accessor for attribute 'uid'.
     *
     * @return
     *       current value of 'uid'
     */
    public String getUid() {
        return uid;
    }

    /**
     * Setter accessor for attribute 'uid'.
     * @param uid
     * 		new value for 'uid '
     */
    public void setUid(String uid) {
        this.uid = uid;
    }

    /**
     * Getter accessor for attribute 'enable'.
     *
     * @return
     *       current value of 'enable'
     */
    public boolean isEnable() {
        return enable;
    }

    /**
     * Setter accessor for attribute 'enable'.
     * @param enable
     * 		new value for 'enable '
     */
    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    /**
     * Getter accessor for attribute 'description'.
     *
     * @return
     *       current value of 'description'
     */
    public String getDescription() {
        return description;
    }

    /**
     * Setter accessor for attribute 'description'.
     * @param description
     * 		new value for 'description '
     */
    public void setDescription(String description) {
        this.description = description;
    }

    /**
     * Getter accessor for attribute 'group'.
     *
     * @return
     *       current value of 'group'
     */
    public String getGroup() {
        return group;
    }

    /**
     * Setter accessor for attribute 'group'.
     * @param group
     * 		new value for 'group '
     */
    public void setGroup(String group) {
        this.group = group;
    }

    /**
     * Getter accessor for attribute 'permissions'.
     *
     * @return
     *       current value of 'permissions'
     */
    public List<String> getPermissions() {
        return permissions;
    }

    /**
     * Setter accessor for attribute 'permissions'.
     * @param permissions
     * 		new value for 'permissions '
     */
    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }

    /**
     * Getter accessor for attribute 'flippingStrategy'.
     *
     * @return
     *       current value of 'flippingStrategy'
     */
    public FlippingStrategyApiBean getFlippingStrategy() {
        return flippingStrategy;
    }

    /**
     * Setter accessor for attribute 'flippingStrategy'.
     * @param flippingStrategy
     * 		new value for 'flippingStrategy '
     */
    public void setFlippingStrategy(FlippingStrategyApiBean flippingStrategy) {
        this.flippingStrategy = flippingStrategy;
    }

    /**
     * Getter accessor for attribute 'customProperties'.
     *
     * @return
     *       current value of 'customProperties'
     */
    public Map<String, PropertyApiBean> getCustomProperties() {
        return customProperties;
    }

    /**
     * Setter accessor for attribute 'customProperties'.
     * @param customProperties
     * 		new value for 'customProperties '
     */
    public void setCustomProperties(Map<String, PropertyApiBean> customProperties) {
        this.customProperties = customProperties;
    }
}
