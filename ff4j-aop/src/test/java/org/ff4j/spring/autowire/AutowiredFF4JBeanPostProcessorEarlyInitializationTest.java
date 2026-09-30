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

import org.ff4j.FF4j;
import org.ff4j.core.Feature;
import org.ff4j.property.PropertyInt;
import org.junit.jupiter.api.Assertions;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.annotation.AnnotationConfigApplicationContext;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.ComponentScan;

import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.Set;

/**
 * Checks that {@link AutowiredFF4JBeanPostProcessor} does not force the early instantiation of the {@code FF4j} bean.
 *
 * <p>When the post-processor injects {@code FF4j} eagerly, Spring has to create that bean (and its dependencies) while
 * the {@link BeanPostProcessor} instances are still being registered. Those beans are then not eligible for processing
 * by all post-processors, which is exactly what {@code BeanPostProcessorChecker} reports as a WARN at startup. The
 * assertions below verify the observable consequence of that problem: every application bean must go through the
 * regular post-processing phase.</p>
 */
class AutowiredFF4JBeanPostProcessorEarlyInitializationTest {

    private static final String FEATURE_NAME = "awesomeFeature";

    private static final String PROPERTY_NAME = "awesomeProperty";

    @Test
    void ff4jBeanAndItsDependenciesAreProcessedByAllBeanPostProcessors() {
        try (AnnotationConfigApplicationContext ctx = new AnnotationConfigApplicationContext(TestConfiguration.class)) {
            Set<String> processed = ctx.getBean(RecordingBeanPostProcessor.class).getProcessedBeanNames();

            Assertions.assertTrue(processed.contains("ff4j"),
                    "The 'ff4j' bean must be created after all BeanPostProcessors are registered, "
                            + "otherwise Spring logs a BeanPostProcessorChecker WARN. Processed beans: " + processed);
            Assertions.assertTrue(processed.contains("ff4jDependency"),
                    "Dependencies of the 'ff4j' bean must also be processed by all BeanPostProcessors. "
                            + "Processed beans: " + processed);
        }
    }

    @Test
    void annotatedFieldsAreStillInjected() {
        try (AnnotationConfigApplicationContext ctx = new AnnotationConfigApplicationContext(TestConfiguration.class)) {
            AnnotatedBean bean = ctx.getBean(AnnotatedBean.class);

            Assertions.assertNotNull(bean.getFeature());
            Assertions.assertTrue(bean.getFeature().isEnable());
            Assertions.assertTrue(bean.isFeatureEnabled());
            Assertions.assertEquals(Integer.valueOf(42), bean.getProperty());
        }
    }

    @Test
    void annotatedFieldsAreInjectedWhenFF4jIsProvidedProgrammatically() {
        AutowiredFF4JBeanPostProcessor postProcessor = new AutowiredFF4JBeanPostProcessor(newFF4j());
        AnnotatedBean bean = new AnnotatedBean();

        postProcessor.postProcessAfterInitialization(bean, "annotatedBean");

        Assertions.assertTrue(bean.isFeatureEnabled());
        Assertions.assertEquals(Integer.valueOf(42), bean.getProperty());
    }

    @Test
    void missingFF4jInstanceIsReportedExplicitly() {
        AutowiredFF4JBeanPostProcessor postProcessor = new AutowiredFF4JBeanPostProcessor();

        Assertions.assertThrows(IllegalStateException.class,
                () -> postProcessor.postProcessAfterInitialization(new AnnotatedBean(), "annotatedBean"));
    }

    private static FF4j newFF4j() {
        FF4j ff4j = new FF4j();
        ff4j.createFeature(new Feature(FEATURE_NAME, true));
        ff4j.createProperty(new PropertyInt(PROPERTY_NAME, 42));
        return ff4j;
    }

    /**
     * Not annotated with {@code @Configuration} on purpose: this class lives in a package covered by the
     * {@code <context:component-scan base-package="org.ff4j"/>} declared by other tests, and must therefore stay
     * invisible to component scanning. It is registered explicitly instead.
     */
    @ComponentScan("org.ff4j.spring.autowire")
    static class TestConfiguration {

        @Bean
        RecordingBeanPostProcessor recordingBeanPostProcessor() {
            return new RecordingBeanPostProcessor();
        }

        @Bean
        FF4jDependency ff4jDependency() {
            return new FF4jDependency();
        }

        @Bean
        FF4j ff4j(FF4jDependency dependency) {
            Assertions.assertNotNull(dependency);
            return newFF4j();
        }

        @Bean
        AnnotatedBean annotatedBean() {
            return new AnnotatedBean();
        }
    }

    /**
     * Marker dependency of the {@code ff4j} bean, used to detect early instantiation.
     */
    static class FF4jDependency {
    }

    /**
     * Records every bean going through the regular post-processing phase.
     */
    static class RecordingBeanPostProcessor implements BeanPostProcessor {

        private final Set<String> processedBeanNames = new LinkedHashSet<>();

        @Override
        public Object postProcessAfterInitialization(Object bean, String beanName) {
            processedBeanNames.add(beanName);
            return bean;
        }

        Set<String> getProcessedBeanNames() {
            return Collections.unmodifiableSet(processedBeanNames);
        }
    }

    /**
     * Bean relying on FF4J annotation-driven injection.
     */
    static class AnnotatedBean {

        @FF4JFeature(FEATURE_NAME)
        private Feature feature;

        @FF4JFeature(FEATURE_NAME)
        private boolean featureEnabled;

        @FF4JProperty(PROPERTY_NAME)
        private Integer property;

        Feature getFeature() {
            return feature;
        }

        boolean isFeatureEnabled() {
            return featureEnabled;
        }

        Integer getProperty() {
            return property;
        }
    }

}
