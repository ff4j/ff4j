package org.ff4j.services;

import org.apache.commons.lang3.StringUtils;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.*;
import org.ff4j.property.store.InMemoryPropertyStore;
import org.ff4j.property.util.PropertyFactory;
import org.ff4j.services.model.FeatureActions;
import org.ff4j.store.InMemoryFeatureStore;
import org.skyscreamer.jsonassert.JSONAssert;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.test.context.ContextConfiguration;

import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import static org.assertj.core.api.Assertions.assertThat;
import static org.ff4j.services.utils.JsonUtils.GSON;

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

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
@ContextConfiguration(classes = {CucumberConfiguration.class})
public class AbstractStepDef {

    @Autowired
    protected FF4j ff4j;
    protected Throwable exception;
    protected Object actualResponse;

    protected void createFeatures(List<FeaturePojo> features) {
        for (FeaturePojo featurePojo : features) {
            Feature feature = new Feature(featurePojo.getUid(), Boolean.valueOf(featurePojo.getEnable()),
                    featurePojo.getDescription(), featurePojo.getGroup(),
                    Arrays.asList(featurePojo.getPermissions().split(",")));
            createFeature(feature);
        }
    }

    protected void createProperties(List<PropertyPojo> properties) {
        for (PropertyPojo propertyPojo : properties) {
            Property<?> property = asProperty(propertyPojo.getName(), propertyPojo.getType(), propertyPojo.getValue(),
                    propertyPojo.getDescription(),
                    StringUtils.isNotBlank(propertyPojo.getFixedValueCSV()) ? new HashSet<String>(Arrays.asList(propertyPojo.getFixedValueCSV().split(","))) : null);
            createProperty(property);
        }
    }

    private void createProperty(Property<?> property) {
        ff4j.createProperty(property);
    }

    protected void createFeature(Feature feature) {
        ff4j.createFeature(feature);
    }

    protected void clearFeatureStore() {
        ff4j.setPropertiesStore(new InMemoryPropertyStore());
        ff4j.setFeatureStore(new InMemoryFeatureStore());
    }

    protected void clearPropertyStore() {
        ff4j.setPropertiesStore(new InMemoryPropertyStore());
        ff4j.setFeatureStore(new InMemoryFeatureStore());
    }

    protected Property<?> asProperty(String name, String type, String value, String description, Set<String> fixedValues) {
        return PropertyFactory.createProperty(name, getType(type), value, description, fixedValues);
    }

    protected void assertException(String className) throws ClassNotFoundException {
        assertThat(exception).isInstanceOf(Class.forName(className));
    }

    protected void assertUpdated() {
        assertThat(actualResponse).isEqualTo(FeatureActions.UPDATED);
    }

    protected void assertCreated() {
        assertThat(actualResponse).isEqualTo(FeatureActions.CREATED);
    }

    protected void assertStrictResponse(String expectedResponse) {
        JSONAssert.assertEquals(expectedResponse, GSON.toJson(actualResponse), true);
    }

    protected void assertFalse() {
        assertThat(Boolean.parseBoolean(actualResponse.toString())).isFalse();
    }

    protected void assertTrue() {
        assertThat(Boolean.parseBoolean(actualResponse.toString())).isTrue();
    }

    protected void assertLenientResponse(String expectedResponse) {
        JSONAssert.assertEquals(expectedResponse, GSON.toJson(actualResponse), false);
    }

    private String getType(String name) {
        PropertyValueEnum propertyEnum = PropertyValueEnum.getEnum(name);
        switch (propertyEnum) {
            case INT:
            case LONG:
            case STRING:
            case BOOLEAN:
                return propertyEnum.getType();
            default:
                return null;
        }
    }

    private enum PropertyValueEnum {
        INT("int", PropertyInt.class.getName()),
        LONG("long", PropertyLong.class.getName()),
        STRING("string", PropertyString.class.getName()),
        BOOLEAN("boolean", PropertyBoolean.class.getName());

        private String name;
        private String type;

        public String getName() {
            return name;
        }

        public String getType() {
            return type;
        }

        PropertyValueEnum(String name, String type) {
            this.name = name;
            this.type = type;
        }

        public static PropertyValueEnum getEnum(String name) {
            for (PropertyValueEnum propertyValueEnum : PropertyValueEnum.values()) {
                if (StringUtils.isNotBlank(name) && name.equalsIgnoreCase(propertyValueEnum.getName())) {
                    return propertyValueEnum;
                }
            }
            throw new UnsupportedOperationException("property " + name + " not found");
        }
    }

    protected class FeaturePojo {
        private String uid;
        private String enable;
        private String description;
        private String group;
        private String permissions;

        public String getUid() {
            return uid;
        }

        public void setUid(String uid) {
            this.uid = uid;
        }

        public String getEnable() {
            return enable;
        }

        public void setEnable(String enable) {
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

        public String getPermissions() {
            return permissions;
        }

        public void setPermissions(String permissions) {
            this.permissions = permissions;
        }
    }

    protected class PropertyPojo {
        private String name;
        private String description;
        private String type;
        private String value;
        private String fixedValueCSV;

        public String getName() {
            return name;
        }

        public void setName(String name) {
            this.name = name;
        }

        public String getDescription() {
            return description;
        }

        public void setDescription(String description) {
            this.description = description;
        }

        public String getType() {
            return type;
        }

        public void setType(String type) {
            this.type = type;
        }

        public String getValue() {
            return value;
        }

        public void setValue(String value) {
            this.value = value;
        }

        public String getFixedValueCSV() {
            return fixedValueCSV;
        }

        public void setFixedValueCSV(String fixedValueCSV) {
            this.fixedValueCSV = fixedValueCSV;
        }
    }
}
