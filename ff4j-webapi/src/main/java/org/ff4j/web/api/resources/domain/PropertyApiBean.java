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

import org.ff4j.property.Property;
import org.ff4j.property.util.PropertyFactory;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonProperty;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import io.swagger.annotations.ApiModel;
import io.swagger.annotations.ApiModelProperty;

/**
 * Abstract representation of {@link Property} as webbean.
 *
 * @author Cedrick Lunven (@clunven)</a>
 */
@ApiModel(value = "propertyApiBean", description = "ff4j property representation" )
@JsonInclude(Include.NON_NULL)
public class PropertyApiBean {
    
    /** unique identifier for the property. */
    @JsonProperty("name")
    @ApiModelProperty( value = "unique property name", required = true )
    private String name;
    
    /** property description if exist. */
    @JsonProperty("description")
    @ApiModelProperty( value = "property description", required = false )
    private String description;
    
    /** nature of the property (classname). */
    @JsonProperty("type")
    @ApiModelProperty( value = "classname of implementation", required = true )
    private String type;
    
    /** Value as String. */
    @JsonProperty("value")
    @ApiModelProperty( value = "value", required = true )
    private String value;
    
    /** Fixed values as String. */
    @JsonProperty("fixedValues")
    @ApiModelProperty( value = "fixedValues", required = false )
    private Set < String > fixedValues = new HashSet<String>();

    /**
     * Default constructor for instrospection.
     */
    public PropertyApiBean() {
    }
    
    /**
     * Initialization from Property.
     *
     * @param property
     *      target property
     */
    public PropertyApiBean(Property<?> property) {
        if (property != null) {
            this.name        = property.getName();
            this.description = property.getDescription();
            this.type        = property.getType();
            this.value       = property.asString();
            if (property.getFixedValues() != null) {
                for (Object fv : property.getFixedValues()) {
                    fixedValues.add(fv.toString());
                }
            }
        }
    }
    
    public Property< ? > asProperty() {
        return PropertyFactory.createProperty(name, type, value, description, fixedValues);
    }

    /**
     * Getter accessor for attribute 'name'.
     *
     * @return
     *       current value of 'name'
     */
    public String getName() {
        return name;
    }

    /**
     * Setter accessor for attribute 'name'.
     * @param name
     * 		new value for 'name '
     */
    public void setName(String name) {
        this.name = name;
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
     * Getter accessor for attribute 'value'.
     *
     * @return
     *       current value of 'value'
     */
    public String getValue() {
        return value;
    }

    /**
     * Setter accessor for attribute 'value'.
     * @param value
     * 		new value for 'value '
     */
    public void setValue(String value) {
        this.value = value;
    }

    /**
     * Getter accessor for attribute 'fixedValues'.
     *
     * @return
     *       current value of 'fixedValues'
     */
    public Set<String> getFixedValues() {
        return fixedValues;
    }

    /**
     * Setter accessor for attribute 'fixedValues'.
     * @param fixedValues
     * 		new value for 'fixedValues '
     */
    public void setFixedValues(Set<String> fixedValues) {
        this.fixedValues = fixedValues;
    }
    
}
