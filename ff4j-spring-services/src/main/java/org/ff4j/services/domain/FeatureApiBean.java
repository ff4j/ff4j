package org.ff4j.services.domain;

/*
 * #%L
 * ff4j-spring-services
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


import org.ff4j.core.Feature;
import org.ff4j.property.Property;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureApiBean implements Serializable {

    private static final long serialVersionUID = -4977143873952901044L;

    private String uid;

    private boolean enable;

    private String description;

    private String group;

    private List<String> permissions = new ArrayList<String>();

    private FlippingStrategyApiBean flippingStrategy;

    private Map<String, PropertyApiBean> customProperties = new HashMap<String, PropertyApiBean>();

    public FeatureApiBean() {
        super();
    }

    public FeatureApiBean(Feature f) {
        this.uid = f.getUid();
        this.enable = f.isEnable();
        this.description = f.getDescription();
        this.group = f.getGroup();
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

    public String getUid() {
        return uid;
    }

    public void setUid(String uid) {
        this.uid = uid;
    }

    public boolean isEnable() {
        return enable;
    }

    public void setEnable(boolean enable) {
        this.enable = enable;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getGroup() {
        return group;
    }

    public void setGroup(String group) {
        this.group = group;
    }

    public List<String> getPermissions() {
        return permissions;
    }

    public void setPermissions(List<String> permissions) {
        this.permissions = permissions;
    }

    public FlippingStrategyApiBean getFlippingStrategy() {
        return flippingStrategy;
    }

    public void setFlippingStrategy(FlippingStrategyApiBean flippingStrategy) {
        this.flippingStrategy = flippingStrategy;
    }

    public Map<String, PropertyApiBean> getCustomProperties() {
        return customProperties;
    }

    public void setCustomProperties(Map<String, PropertyApiBean> customProperties) {
        this.customProperties = customProperties;
    }
}
