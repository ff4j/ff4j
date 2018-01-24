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
import static org.ff4j.utils.MappingUtil.instanceFlippingStrategy;
import static org.ff4j.utils.MappingUtil.toMap;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import javax.lang.model.type.NullType;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.ff4j.FF4j;
import org.ff4j.core.FlippingExecutionContext;
import org.ff4j.core.FlippingStrategy;
import org.ff4j.utils.Util;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.framework.Advised;
import org.springframework.aop.framework.AopProxyUtils;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.ApplicationContext;
import org.springframework.stereotype.Component;
import org.springframework.stereotype.Repository;
import org.springframework.stereotype.Service;

/**
 * At runtime check presence of annotation @{Flip}, then evaluate if the related feature id is enabled.
 * If the feature is enabled, the implementation is route to the correct implementation. 
 * 
 * @author Cedrick LUNVEN (@clunven)
 */
@Component("ff.advisor")
public class FeatureAdvisor implements MethodInterceptor {

    /** Log with target className. */
    private final static Logger LOGGER = LoggerFactory.getLogger(FeatureAdvisor.class);
    
    /** Spring Application Context. */
    @Autowired
    private ApplicationContext appCtx;

    /** Injection of current FF4J bean. */
    @Autowired
    private FF4j ff4j;

    /** {@inheritDoc} */
    @Override
    public Object invoke(final MethodInvocation mi) throws Throwable {
        Flip ff4jAnnotation = getFF4jAnnotation(mi);
        
        // Method is annotated
        if (ff4jAnnotation != null) {
        	
        	String alterBean    = ff4jAnnotation.alterBean();
        	Class<?> alterClazz = ff4jAnnotation.alterClazz();
        	boolean isFeatureToggled = check(ff4jAnnotation, mi);
        	
        	// Would like to skip if feature is Disable
        	if (!Util.hasLength(alterBean) && !Util.isValidClass(alterClazz) && !isFeatureToggled) {
        		return null;
        	}
        	
        	// Feature is 'ON'
        	if (isFeatureToggled) {
	            // Do we use the alter bean defined in the annotation ?
	            if (Util.hasLength(alterBean)
	                    // Bean name exist
	                    & appCtx.containsBean(alterBean)   
	                    // Bean name is not the same as current
	                    & !alterBean.equals(getExecutedBeanName(mi))) {
	                return invokeAlterBean(mi, alterBean);
	            }
	            
	            // Or else do we use the alter class defined in the annotation ?
	            if (Util.isValidClass(alterClazz) 
	                    // Alter class is not the same as current
	                    & (alterClazz != getExecutedClass(mi))) {
	                return invokeAlterClazz(mi, ff4jAnnotation);
	            }
        	}
        }
        // No feature toggle (no annotation nor feature OFF)
        return mi.proceed();
    }
   
    /**
     * Call if Flipped based on different parameters of the annotation
     * 
     * @param ff
     *            annotation over current method
     * @param context
     * @return if flippinf should be considere
     */
    protected boolean check(Flip ff, MethodInvocation mi) {
        // Retrieve optional context with ThreadLocal
        FlippingExecutionContext context = getFlippingContext(ff, mi);
        
        // Check ff4j
        String featureId = ff.name();
        if (ff.flippingStrategy() != NullType.class) {
            String fsClassName  = ff.flippingStrategy().getName();
            FlippingStrategy fs = instanceFlippingStrategy(featureId, fsClassName, toMap(ff.flippingInitParams()));
            return getFf4j().checkOveridingStrategy(featureId, fs, context);
        }
        return getFf4j().check(featureId, context);
    }
    
    /**
     * Pick annotation from method or class.
     *
     * @param method
     *      current method
     * @return
     *      the associated annotation
     */
    protected Flip getFF4jAnnotation(MethodInvocation mi) {
        if (mi.getMethod().isAnnotationPresent(Flip.class)) {
            return mi.getMethod().getAnnotation(Flip.class);
        }
        Class <?> currentInterface = mi.getMethod().getDeclaringClass();
        if (currentInterface.isAnnotationPresent(Flip.class)) {
            return currentInterface.getAnnotation(Flip.class);
        }
        Class <?> currentImplementation = getExecutedClass(mi);
        if (currentImplementation.isAnnotationPresent(Flip.class)) {
            return currentImplementation.getAnnotation(Flip.class);
        }
        return null;
    }
    
    /**
     * Retriveve {@link FlippingExecutionContext} from FF4J or as parameter.
     * 
     * @param ff
     *      current annotation
     * @param mi
     *      invocation
     * @return
     */
    protected FlippingExecutionContext getFlippingContext(Flip ff, MethodInvocation mi) {
        switch (ff.contextLocation()) {
            case FF4J:
                return getFf4j().getCurrentContext();
            case PARAMETER:
                // We are looking for the first parameter (not argument!) 
                // that is an instance of FlippingExecutionContext
                int p = 0;
                for (Class<?> cls : mi.getMethod().getParameterTypes()) {
                    if (FlippingExecutionContext.class.isAssignableFrom(cls)) {
                        return FlippingExecutionContext.class.cast(mi.getArguments()[p]);
                    }
                    p++;
                }
            case NONE:
            default: return null;
        }
    }
    
    /**
     * Find current class based on the {@link MethodInvocation} and passing throug AOP Proxies.
     *
     * @param pMInvoc
     *      current method invocation
     * @return
     *      current class of raise error if static
     */
    protected Class<?> getExecutedClass(MethodInvocation pMInvoc) {
        Class<?> executedClass = null;
        Object ref = pMInvoc.getThis();
        if (ref != null) {
            executedClass = AopUtils.getTargetClass(ref); 
        }
        if (executedClass == null) {
            throw new IllegalArgumentException("ff4j-aop: Static methods cannot be feature flipped");
        }
        return executedClass;
    }

    /**
     * Find bean name related to current method invocation.
     * @param pMInvoc
     *      current method invocation
     * @return
     *      bean name related to this method
     */
    protected String getExecutedBeanName(MethodInvocation mi) {
        Class<?> targetClass = getExecutedClass(mi);
        Component component = targetClass.getAnnotation(Component.class);
        if (component != null) {
            return component.value();
        }
        Service service = targetClass.getAnnotation(Service.class);
        if (service != null) {
            return service.value();
        }
        Repository repo = targetClass.getAnnotation(Repository.class);
        if (repo != null) {
            return repo.value();
        }
        
        // There is no annotation on the bean, still be declared in applicationContext.xml
        try {
            // Use BeanDefinition names to loop on each bean and fetch target if proxified
            for(String beanName :  appCtx.getBeanDefinitionNames()) {
                Object bean = appCtx.getBean(beanName);
                if (AopUtils.isJdkDynamicProxy(bean)) {
                   bean = ((Advised) bean).getTargetSource().getTarget();
                }
                if (bean != null && bean.getClass().isAssignableFrom(targetClass)) {
                    return beanName;
                }
            }
        } catch (Exception e) {
            throw new RuntimeException("ff4j-aop: Cannot read bheind proxy target", e);
        }
        throw new IllegalArgumentException("ff4j-aop: Feature bean must be annotated as a Service or a Component");
    }
    
    private IllegalArgumentException makeIllegalArgumentException(String message, Exception exception) {
        return new IllegalArgumentException(message, exception);
    }

    /**
     * Invoke another Bean for the current Method.
     * 
     * @param mi
     *      current method invocation
     * @param alterBean
     *      target bean
     * @return
     *      return of invocation
     * @throws Throwable
     *      erros occured
     */
    protected Object invokeAlterBean(final MethodInvocation mi, String alterBeanName) throws Throwable {
        Method method = mi.getMethod();
        try {
            LOGGER.debug("FeatureFlipping on method:{} class:{}", method.getName(), method.getDeclaringClass().getName());
            Object alterbean = appCtx.getBean(alterBeanName, method.getDeclaringClass());
            return method.invoke(alterbean, mi.getArguments());
        } catch (InvocationTargetException invocationTargetException) {
            if(!ff4j.isAlterBeanThrowInvocationTargetException() && invocationTargetException.getCause() != null) {
                throw invocationTargetException.getCause();
            }
            throw makeIllegalArgumentException("ff4j-aop: Cannot invoke method " + method.getName() + " on bean " + alterBeanName, invocationTargetException);
        } catch (Exception exception) {
            throw makeIllegalArgumentException("ff4j-aop: Cannot invoke method " + method.getName() + " on bean " + alterBeanName, exception);
        }
    }

    /**
     * Invoke alter class.
     *
     * @param mi
     *      method invocation
     * @param ff
     *      ff4j annotation
     * @return
     *      object returned by the 
     * @throws Throwable
     *      error during invocation
     */
    protected Object invokeAlterClazz(final MethodInvocation mi, Flip ff) throws Throwable {
        Class<?> alterClazz     = ff.alterClazz();
        Method   method         = mi.getMethod();
        Class<?> declaringClass = method.getDeclaringClass();
        try {
            // Spring context may have a bean of expected type and priority of get instance
            for (Object bean : appCtx.getBeansOfType(declaringClass).values()) {
                // Correct bean implementing the same class, or proxy of existing class
                if (AopUtils.isJdkDynamicProxy(bean) &&  ((Advised) bean).getTargetSource().getTarget().getClass().equals(alterClazz) ||
                    AopProxyUtils.ultimateTargetClass(bean).equals(alterClazz)) {
                    return mi.getMethod().invoke(bean, mi.getArguments());
                }
            }
            // Otherwise instanciate manually
            return mi.getMethod().invoke(ff.alterClazz().newInstance(), mi.getArguments());
        } catch (IllegalAccessException e) {
            throw makeIllegalArgumentException("ff4j-aop: Cannot invoke " + method.getName() + " on alterbean " + declaringClass
                    + " please check visibility", e);
        } catch (InvocationTargetException invocationTargetException) {
            if(!ff4j.isAlterBeanThrowInvocationTargetException() && invocationTargetException.getCause() != null) {
                throw invocationTargetException.getCause();
            }
            throw makeIllegalArgumentException("ff4j-aop: Cannot invoke " + method.getName() + " on alterbean " + declaringClass
                    + " please check signatures", invocationTargetException);
        } catch (Exception exception) {
            throw makeIllegalArgumentException("ff4j-aop: Cannot invoke " + method.getName() + " on alterbean " + declaringClass
                    + " please check signatures", exception);
        }
    }

    /**
     * Getter accessor for attribute 'ff4j'.
     * 
     * @return current value of 'ff4j'
     */
    public FF4j getFf4j() {
        return ff4j;
    }

    /**
     * Setter accessor for attribute 'ff4j'.
     * @param ff4j
     *      new value for 'ff4j '
     */
    public void setFf4j(FF4j ff4j) {
        this.ff4j = ff4j;
    }

}