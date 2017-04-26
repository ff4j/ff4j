package org.ff4j.aop;

/*
 * #%L ff4j-aop %% Copyright (C) 2013 Ff4J %% Licensed under the Apache License, Version 2.0 (the "License"); you may not use this
 * file except in compliance with the License. You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS"
 * BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the specific language
 * governing permissions and limitations under the License. #L%
 */

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.springframework.aop.TargetSource;
import org.springframework.aop.framework.autoproxy.AbstractAutoProxyCreator;
import org.springframework.stereotype.Component;

/**
 * Spring bean used to scan classpath and create dynamic proxy on annotated beans (@Flip).
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
@Component("ff.autoproxy")
public class FeatureAutoProxy extends AbstractAutoProxyCreator {

    /** Serial number. */
    private static final long serialVersionUID = -364406999854610869L;

    /** Cache to avoid two-passes on same interfaces. */
    private final Map<String, Boolean> processedInterface = new HashMap<String, Boolean>();

    /**
     * Default constructor invoked by spring.
     */
    public FeatureAutoProxy() {
        // Define scanner for classes at startup
        setInterceptorNames(getBeanNameOfFeatureAdvisor());
    }
    
    /**
     * Read advisor bean name.
     *  
     * @return
     *      id of {@link FeatureAdvisor} bean
     */
    private String getBeanNameOfFeatureAdvisor() {
        return FeatureAdvisor.class.getAnnotation(Component.class).value();
    }

    /** {@inheritDoc} */
    @Override
    protected Object[] getAdvicesAndAdvisorsForBean(Class<?> beanClass, String beanName, TargetSource targetSource) {
        // Do not used any AOP here as still working with classes and not objects
        if (!beanClass.isInterface() && beanClass.getInterfaces() != null) {
            for (Class<?> currentInterface : beanClass.getInterfaces()) {
                Object[] r = scanInterface(currentInterface);
                if (r != null) {
                    return r;
                }
            }
        }
        return DO_NOT_PROXY;
    }

    /**
     * Add current annotated interface.
     *
     * @param currentInterface
     *          class to be scanned
     * @return
     */
    private Object[] scanInterface(Class<?> currentInterface) {
        String currentInterfaceName = currentInterface.getCanonicalName();
        // Do not scan internals
        if (isJdkInterface(currentInterfaceName)) {
            return null;
        }
        // Never scanned, scan first time
        if (!processedInterface.containsKey(currentInterfaceName)) {
            return scanInterfaceForAnnotation(currentInterface, currentInterfaceName);
        }
        // Already scanned and flipped do not add interceptors 
        Boolean isInterfaceFlipped = processedInterface.get(currentInterfaceName);
        return isInterfaceFlipped ? PROXY_WITHOUT_ADDITIONAL_INTERCEPTORS : null;
    }
    
    /**
     * Avoid JDK classes.
     *
     * @param currentInterfaceName
     * @return
     */
    private boolean isJdkInterface(String currentInterfaceName) {
        return currentInterfaceName.startsWith("java.");
    }
    
    private Object[] scanInterfaceForAnnotation(Class<?> currentInterface, String currentInterfaceName) {
        // Interface never scan 
        if (currentInterface.isAnnotationPresent(Flip.class)) {
            processedInterface.put(currentInterfaceName, true);
            return PROXY_WITHOUT_ADDITIONAL_INTERCEPTORS;
            
         } else {
             // not found on bean, check methods
             for (Method method : currentInterface.getDeclaredMethods()) {
                 if (method.isAnnotationPresent(Flip.class)) {
                     processedInterface.put(currentInterfaceName, true);
                     return PROXY_WITHOUT_ADDITIONAL_INTERCEPTORS;
                 }
             }
         }
        // annotation has not been found
        processedInterface.put(currentInterfaceName, false);
        return null;
    }
}