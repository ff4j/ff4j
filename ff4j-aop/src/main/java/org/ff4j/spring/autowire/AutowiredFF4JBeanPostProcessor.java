package org.ff4j.spring.autowire;

/*-
 * #%L
 * ff4j-aop
 * %%
 * Copyright (C) 2013 - 2024 FF4J
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
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.ObjectProvider;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.Role;
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
 * <p>The {@link FF4j} instance is resolved lazily, and only when a bean actually declares a field annotated with
 * {@link FF4JFeature} or {@link FF4JProperty}. Injecting it eagerly would force Spring to instantiate the {@code FF4j}
 * bean (and its whole dependency graph) while {@link BeanPostProcessor} instances are still being registered, making
 * those beans ineligible for processing by all post-processors and producing {@code BeanPostProcessorChecker} warnings
 * at startup.</p>
 *
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Component("ff4j.autowiringpostprocessor")
@Role(BeanDefinition.ROLE_INFRASTRUCTURE)
public class AutowiredFF4JBeanPostProcessor implements BeanPostProcessor, BeanFactoryAware {

    /**
     * Logger for this class.
     */
    protected final Log logger = LogFactory.getLog(getClass());

    /**
     * Current FF4J bean, when provided explicitly.
     */
    private FF4j ff4j;

    /**
     * Lazy provider for the FF4J bean, used when the post-processor is managed by a Spring container.
     */
    private ObjectProvider<FF4j> ff4jProvider;

    /**
     * Default constructor, the {@link FF4j} instance is then resolved lazily from the bean factory.
     */
    public AutowiredFF4JBeanPostProcessor() {
        // FF4j is resolved lazily, see resolveFF4j()
    }

    /**
     * Constructor for programmatic usage with an explicit {@link FF4j} instance.
     *
     * @param ff4j
     *      current FF4J instance
     */
    public AutowiredFF4JBeanPostProcessor(FF4j ff4j) {
        this.ff4j = ff4j;
    }

    /**
     * {@inheritDoc}
     */
    @Override
    public void setBeanFactory(BeanFactory beanFactory) throws BeansException {
        this.ff4jProvider = beanFactory.getBeanProvider(FF4j.class);
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     *
     * @param ff4j
     *      new value for 'ff4j'
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }

    /**
     * Resolve the {@link FF4j} instance, only invoked when an annotated field has been found.
     *
     * @return
     *      current FF4J instance
     */
    private FF4j resolveFF4j() {
        if (ff4j == null && ff4jProvider != null) {
            ff4j = ff4jProvider.getObject();
        }
        if (ff4j == null) {
            throw new IllegalStateException("Cannot autowire FF4J annotated fields as no FF4j instance is available,"
                    + " please declare a bean of type org.ff4j.FF4j in your context");
        }
        return ff4j;
    }

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
                injectValue(field, bean, featureName, Boolean.valueOf(feature.isEnable()));
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
        FF4j currentFF4j = resolveFF4j();
        if (!currentFF4j.getFeatureStore().exist(featureName)) {
            if (required) {
                throw new IllegalArgumentException("Cannot autowiring field '" + field.getName() + "' with FF4J property as"
                        + " target feature has not been found");
            } else {
                logger.warn("Feature '" + featureName + "' has not been found but not required");
                return null;
            }
        }
        return currentFF4j.getFeatureStore().read(featureName);
    }

    private Property<?> readProperty(Field field, String propertyName, boolean required) {
        FF4j currentFF4j = resolveFF4j();
        if (!currentFF4j.getPropertiesStore().existProperty(propertyName)) {
            if (required) {
                throw new IllegalArgumentException("Cannot autowiring field '" + field.getName() + "' with FF4J property as"
                        + " target property has not been found");
            } else {
                logger.warn("Property '" + propertyName + "' has not been found but not required");
                return null;
            }
        }
        return currentFF4j.getPropertiesStore().readProperty(propertyName);
    }

}
