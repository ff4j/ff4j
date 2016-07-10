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


import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class GroupDescApiBean implements Serializable {

    private static final long serialVersionUID = -7339190302097692175L;

    private String groupName;

    private List<String> features = new ArrayList<String>();

    public GroupDescApiBean() {
        super();
    }

    public GroupDescApiBean(String groupName, List<String> names) {
        this.groupName = groupName;
        this.features = names;
    }

    public String getGroupName() {
        return groupName;
    }

    public List<String> getFeatures() {
        return features;
    }

    public void setGroupName(String groupName) {
        this.groupName = groupName;
    }

    public void setFeatures(List<String> features) {
        this.features = features;
    }
}
