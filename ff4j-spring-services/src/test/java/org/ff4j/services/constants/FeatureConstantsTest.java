package org.ff4j.services.constants;

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

import org.junit.Test;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;

import static org.assertj.core.api.Assertions.assertThat;

/**
 * @author <a href="mailto:paul58914080@gmail.com">Paul Williams</a>
 */
public class FeatureConstantsTest {
    
    @Test
    public void constructorInvocation() {
        try {
            final Constructor<FeatureConstants> c = FeatureConstants.class.getDeclaredConstructor();
            c.setAccessible(true);
            final FeatureConstants newInstance = c.newInstance();
            assertThat(newInstance).isNull();
        } catch (NoSuchMethodException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        } catch (SecurityException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        } catch (InstantiationException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        } catch (IllegalAccessException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        } catch (IllegalArgumentException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        } catch (InvocationTargetException e) {
            assertThat(e).hasCauseExactlyInstanceOf(UnsupportedOperationException.class);
        }
    }

    @Test
    public void constantValuesTest() {
        assertThat(FeatureConstants.PARAM_GROUP).isEqualTo("groupName");
        assertThat(FeatureConstants.PARAM_ROLE).isEqualTo("role");
        assertThat(FeatureConstants.PARAM_NAME).isEqualTo("name");
        assertThat(FeatureConstants.PARAM_VALUE).isEqualTo("value");
        assertThat(FeatureConstants.PATH_PARAM_UID).isEqualTo("{uid}");
        assertThat(FeatureConstants.PATH_PARAM_ROLE).isEqualTo("{role}");
        assertThat(FeatureConstants.PATH_PARAM_GROUP).isEqualTo("{groupName}");
        assertThat(FeatureConstants.PATH_PARAM_NAME).isEqualTo("{name}");
        assertThat(FeatureConstants.PATH_PARAM_VALUE).isEqualTo("{value}");
        assertThat(FeatureConstants.ROOT).isEqualTo("/api/");
        assertThat(FeatureConstants.RESOURCE_FF4J).isEqualTo("/api/ff4j");
        assertThat(FeatureConstants.RESOURCE_STORE).isEqualTo("/store");
        assertThat(FeatureConstants.RESOURCE_FEATURES).isEqualTo("/features");
        assertThat(FeatureConstants.RESOURCE_GROUPS).isEqualTo("/groups");
        assertThat(FeatureConstants.RESOURCE_FF4J_STORE).isEqualTo("/api/ff4j/store");
        assertThat(FeatureConstants.RESOURCE_FF4J_STORE_FEATURES).isEqualTo("/api/ff4j/store/features");
        assertThat(FeatureConstants.RESOURCE_FF4J_STORE_GROUPS).isEqualTo("/api/ff4j/store/groups");
        assertThat(FeatureConstants.RESOURCE_PROPERTY_STORE).isEqualTo("/propertyStore");
        assertThat(FeatureConstants.RESOURCE_PROPERTIES).isEqualTo("/properties");
        assertThat(FeatureConstants.RESOURCE_PROPERTIES_STORE_PROPERTIES).isEqualTo("/api/ff4j/propertyStore/properties");
        assertThat(FeatureConstants.RESOURCE_FF4J_PROPERTY_STORE).isEqualTo("/api/ff4j/propertyStore");
        assertThat(FeatureConstants.RESOURCE_CLEAR_CACHE).isEqualTo("/clearCache");
        assertThat(FeatureConstants.RESOURCE_FF4J_MONITORING).isEqualTo("/api/ff4j/monitoring");
    }
}
