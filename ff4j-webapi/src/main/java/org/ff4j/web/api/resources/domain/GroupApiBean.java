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

import com.fasterxml.jackson.annotation.JsonProperty;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Class representing a group.
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@ApiModel( value = "groupApiBean", description = "Representation of a group" )
public class GroupApiBean {
    
    /** Name of the group.*/
    @ApiModelProperty( value = "name of current group", required = true )
    @JsonProperty("groupName")
    private String groupName;
    
    /** Feature list. */
    @ApiModelProperty( value = "name of current group", required = true )
    @JsonProperty("features")
    private List < FeatureApiBean > features = new ArrayList<FeatureApiBean>();

    /**
     * Getter accessor for attribute 'groupName'.
     *
     * @return
     *       current value of 'groupName'
     */
    public String getGroupName() {
        return groupName;
    }

    /**
     * Setter accessor for attribute 'groupName'.
     * @param groupName
     * 		new value for 'groupName '
     */
    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    /**
     * Getter accessor for attribute 'features'.
     *
     * @return
     *       current value of 'features'
     */
    public List<FeatureApiBean> getFeatures() {
        return features;
    }

    /**
     * Setter accessor for attribute 'features'.
     * @param features
     * 		new value for 'features '
     */
    public void setFeatures(List<FeatureApiBean> features) {
        this.features = features;
    }
    
    

}
