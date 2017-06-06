package org.ff4j.spring.autowire;

/*
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2015 Ff4J
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


import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.Property;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.stereotype.Component;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

/**
 * When Proxified, analyze bean to eventually invoke ANOTHER implementation (flip up).
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Component("ff4j.autowiringpostprocessor")
public class AutowiredFF4JBeanPostProcessor implements BeanPostProcessor {

    /**
     * Logger for this class.
     */
    protected final Log logger = LogFactory.getLog(getClass());

    /**
     * Injection of current FF4J bean.
     */
    @Autowired
    private FF4j ff4j;

    /**
     * {@inheritDoc}
     */
    @Override
    public Object postProcessBeforeInitialization(Object bean, String beanName) {
        // Nothing to do before initializations, will inject only on post treatment
        return bean;
    }

    /**
     * {@inheritDoc}
     */
    /**
     * {@inheritDoc}
     */
    @Override
    public Object postProcessAfterInitialization(Object bean, String beanName) {
        if (bean == null) return null;
        Class<?> beanClass = bean.getClass();
        Field[] fields = getAllFields(beanClass);
        for (Field field : fields) {
            // Expect to get annnotation Autowired
            if (field.isAnnotationPresent(FF4JProperty.class)) {
                autoWiredProperty(bean, field);
            } else if (field.isAnnotationPresent(FF4JFeature.class)) {
                autoWiredFeature(bean, field);
            }
        }
        return bean;

    }

    //Loops through the class hierarchy of the spring managed bean to get all fields
    private Field[] getAllFields(final Class<?> beanClass) {
        final List<Field> fields = new ArrayList<Field>();
        Class<?> clazz = beanClass;

        while (clazz != Object.class) {
            fields.addAll(Arrays.asList(clazz.getDeclaredFields()));

            clazz = clazz.getSuperclass();
        }

        return fields.toArray(new Field[fields.size()]);
    }

    private void autoWiredFeature(Object bean, Field field) {
        // Find the required and name parameters
        FF4JFeature annFeature = field.getAnnotation(FF4JFeature.class);
        String annValue = annFeature.value();
        String featureName = field.getName();
        if (annValue != null && !"".equals(annValue)) {
            featureName = annValue;
        }
        Feature feature = readFeature(field, featureName, annFeature.required());
        if (feature != null) {
            if (Feature.class.isAssignableFrom(field.getType())) {
                injectValue(field, bean, featureName, feature);
            } else if (Boolean.class.isAssignableFrom(field.getType())) {
                injectValue(field, bean, featureName, new Boolean(feature.isEnable()));
            } else if (boolean.class.isAssignableFrom(field.getType())) {
                injectValue(field, bean, featureName, feature.isEnable());
            } else {
                throw new IllegalArgumentException("Field annotated with @FF4JFeature"
                        + " must inherit from org.ff4j.Feature or be boolean " + field.getType() + " [class=" + bean.getClass().getName()
                        + ", field=" + field.getName() + "]");
            }
        }
    }

    private void autoWiredProperty(Object bean, Field field) {
        // Find the required and name parameters
        FF4JProperty annProperty = field.getAnnotation(FF4JProperty.class);
        String propertyName = StringUtils.hasLength(annProperty.value()) ? annProperty.value() : field.getName();
        Property<?> property = readProperty(field, propertyName, annProperty.required());
        // if not available in store
        if (property != null) {
            if (Property.class.isAssignableFrom(field.getType())) {
                injectValue(field, bean, propertyName, property);
            } else if (property.parameterizedType().isAssignableFrom(field.getType())) {
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Integer.class)
                    && field.getType().equals(int.class) && (null != property.getValue())) {
                // Autoboxing Integer -> Int
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Long.class)
                    && field.getType().equals(long.class) && (null != property.getValue())) {
                // Autoboxing Long -> long
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Double.class)
                    && field.getType().equals(double.class) && (null != property.getValue())) {
                // Autoboxing Double -> double
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Byte.class)
                    && field.getType().equals(byte.class) && (null != property.getValue())) {
                // Autoboxing Byte -> byte
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Boolean.class)
                    && field.getType().equals(boolean.class) && (null != property.getValue())) {
                // Autoboxing Boolean -> boolean
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Short.class)
                    && field.getType().equals(short.class) && (null != property.getValue())) {
                // Autoboxing Short -> short
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Character.class)
                    && field.getType().equals(char.class) && (null != property.getValue())) {
                // Autoboxing Character -> char
                injectValue(field, bean, propertyName, property.getValue());
            } else if (property.parameterizedType().equals(Float.class)
                    && field.getType().equals(float.class) && (null != property.getValue())) {
                // Autoboxing Float -> float
                injectValue(field, bean, propertyName, property.getValue());
            } else {
                throw new IllegalArgumentException("Field annotated with @FF4JProperty"
                        + " must inherit from org.ff4j.property.AbstractProperty or be of type " +
                        property.parameterizedType() + "but is " + field.getType() + " [class=" + bean.getClass().getName()
                        + ", field=" + field.getName() + "]");
            }
        }
    }

    private void injectValue(Field field, Object currentBean, String propName, Object propValue) {
        // Set as true for modifications
        ReflectionUtils.makeAccessible(field);
        // Update property
        ReflectionUtils.setField(field, currentBean, propValue);
        logger.debug("Injection of property '" + propName + "' on " + currentBean.getClass().getName() + "." + field.getName());
    }

    private Feature readFeature(Field field, String featureName, boolean required) {
        if (!ff4j.getFeatureStore().exist(featureName)) {
            if (required) {
                throw new IllegalArgumentException("Cannot autowiring field '" + field.getName() + "' with FF4J property as"
                        + " target feature has not been found");
            } else {
                logger.warn("Feature '" + featureName + "' has not been found but not required");
                return null;
            }
        }
        return ff4j.getFeatureStore().read(featureName);
    }

    private Property<?> readProperty(Field field, String propertyName, boolean required) {
        if (!ff4j.getPropertiesStore().existProperty(propertyName)) {
            if (required) {
                throw new IllegalArgumentException("Cannot autowiring field '" + field.getName() + "' with FF4J property as"
                        + " target property has not been found");
            } else {
                logger.warn("Property '" + propertyName + "' has not been found but not required");
                return null;
            }
        }
        return ff4j.getPropertiesStore().readProperty(propertyName);
    }

}