package org.ff4j.spring.placeholder;

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
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanDefinitionStoreException;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.beans.factory.BeanFactoryAware;
import org.springframework.beans.factory.BeanInitializationException;
import org.springframework.beans.factory.BeanNameAware;
import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanDefinitionVisitor;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.core.Ordered;

/**
 * PostProcessorFactory used to perform replacement of <em>@ff4jProperty{""}</em> in XML files by property value.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
public class FF4jPropertiesPlaceHolderConfigurer implements BeanFactoryPostProcessor, BeanFactoryAware, Ordered, BeanNameAware {

	/** Static commons-log LOG for this class. */
    private static final Log LOGGER = LogFactory.getLog(FF4jPropertiesPlaceHolderConfigurer.class);
    
    /** bean id. */
    private String id;
    
    /** Order to process BeanProcessorFactory. */
    private int order;

    /** Metadata query service interface. */
    private FF4j ff4j;

    /** Service Name. * */
    private String beanName;

    /** Service Factory. * */
    private BeanFactory beanFactory;
    
    /** {@inheritDoc} */
    public final void postProcessBeanFactory(final ConfigurableListableBeanFactory beanFactory) {
        try {
            // 1) Retrieve properties from ff4j
            BeanDefinitionVisitor visitor = new PropertiesPlaceHolderBeanDefinitionVisitor(ff4j);

            // 2) Inject property & features value
            String[] beanNames = beanFactory.getBeanDefinitionNames();
            for (int i = 0; i < beanNames.length; i++) {
                if (beanNames[i].equals(beanName)) {
                    continue;
                }
                BeanDefinition bd = beanFactory.getBeanDefinition(beanNames[i]);
                try {
                    visitor.visitBeanDefinition(bd);
                } catch (BeanDefinitionStoreException ex) {
                    throw new BeanDefinitionStoreException(bd.getResourceDescription(), beanNames[i], ex.getMessage());
                }
            }
        } catch (Exception e) {
            LOGGER.error("Cannot handle placeholding through ff4j : ", e);
            throw new BeanInitializationException("An error occured during substition", e);
        }
    }
    
    /**
     * {@inheritDoc}
     */
    public void setBeanName(String beanName) {
        this.beanName = beanName;
    }
    
    /**
     * {@inheritDoc}
     */
    public void setBeanFactory(BeanFactory arg0) throws BeansException {
        this.beanFactory = arg0;
    }


    /**
     * Getter accessor for attribute 'ff4j'.
     *
     * @return
     *       current value of 'ff4j'
     */
    public FF4j getFf4j() {
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * @param ff4j
     * 		new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }

    /**
     * Getter accessor for attribute 'order'.
     *
     * @return
     *       current value of 'order'
     */
    public int getOrder() {
        return order;
    }

    /**
     * Setter accessor for attribute 'order'.
     * @param order
     * 		new value for 'order '
     */
    public void setOrder(int order) {
        this.order = order;
    }

    /**
     * Getter accessor for attribute 'beanName'.
     *
     * @return
     *       current value of 'beanName'
     */
    public String getBeanName() {
        return beanName;
    }

    /**
     * Getter accessor for attribute 'beanFactory'.
     *
     * @return
     *       current value of 'beanFactory'
     */
    public BeanFactory getBeanFactory() {
        return beanFactory;
    }

    /**
     * Getter accessor for attribute 'id'.
     *
     * @return
     *       current value of 'id'
     */
    public String getId() {
        return id;
    }

    /**
     * Setter accessor for attribute 'id'.
     * @param id
     * 		new value for 'id '
     */
    public void setId(String id) {
        this.id = id;
    }
}
