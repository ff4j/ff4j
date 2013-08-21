package org.ff4j.aop;

import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import javax.lang.model.type.NullType;

import org.aopalliance.intercept.MethodInterceptor;
import org.aopalliance.intercept.MethodInvocation;
import org.ff4j.FF4j;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.aop.framework.Advised;
import org.springframework.aop.support.AopUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.context.ApplicationContext;
import org.springframework.context.ApplicationContextAware;
import org.springframework.stereotype.Component;

/**
 * When Proxified, analyze bean to eventually invoke ANOTHER implementation (flip up). 
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Component("ff.advisor")
public class FeatureAdvisor implements MethodInterceptor, BeanPostProcessor, ApplicationContextAware {
	
	/** Logger for Advisor. */
	final static Logger logger = LoggerFactory.getLogger(FeatureAdvisor.class);
	
	/** Log with target className. */
	private Map <String, Logger> targetLoggers = new HashMap< String, Logger>();
	
	/** Processed Interfaces. */
	private Set < String > targetInterfacesNames = new HashSet<String>();
	
	/** Spring Application Context. */
	private ApplicationContext appCtx;
	
	/** {@inheritDoc} */
	public Object postProcessBeforeInitialization(Object bean, String beanName)
	throws BeansException {
		// Before Initializing allow to check Annotations
		Class<?> target = bean.getClass();
		// Scan interface only once.
		if (!target.isInterface() && target.getInterfaces()!= null) {
			// Get Interface
			for (Class<?> currentInterface : target.getInterfaces()) {
				String currentInterfaceName = currentInterface.getCanonicalName();
				if (!currentInterfaceName.startsWith("java.") && !targetInterfacesNames.contains(currentInterfaceName)) {
					targetInterfacesNames.add(currentInterfaceName);
				}
			}
		}
		return bean;
	}

	/** {@inheritDoc} */
	public Object postProcessAfterInitialization(Object bean, String beanName)
	throws BeansException {
		return bean;
	}

	/** {@inheritDoc} */
	public Object invoke(final MethodInvocation pMInvoc) throws Throwable {
		
		Method method = pMInvoc.getMethod();
		
		// Register logger if require
		if (targetLoggers.get(method.getDeclaringClass().getCanonicalName()) == null) {
			targetLoggers.put(method.getDeclaringClass().getCanonicalName(),
					LoggerFactory.getLogger(method.getDeclaringClass()));
		}
		
		// TargetLogger
		Logger logger = targetLoggers.get(method.getDeclaringClass().getCanonicalName());
		
		// Method exist in class with no annotations
		if (method.isAnnotationPresent(Flip.class)) {
			
			Flip ff = method.getAnnotation(Flip.class);
			
			// Assess if flipped is required upon parameters
			boolean shouldFlip = false;
			if (ff.strategy() != NullType.class) {
				// a strategy has been provided
				if (!"".equals(ff.expression())) {
					// an expression has been provided
					shouldFlip = FF4j.isFlipped(ff.name(), ff.strategy().newInstance(), ff.expression());
				} else {
					// no expression provided, only strategy (value must be provided in ff4j conf file or store)
					shouldFlip = FF4j.isFlipped(ff.name(), ff.strategy().newInstance());
				}
			} else {
				// no strategy, simple flip
				shouldFlip = FF4j.isFlipped(ff.name());
			}
			
			// Locating alternativ target
			if (shouldFlip) {
				// Test by beanName (first priority over alterClazz)
				if (ff.alterBean() != null && !ff.alterBean().isEmpty()) {
					if(shouldCallAlterBeanMethod(pMInvoc, ff.alterBean(), logger)){
						logger.debug("FeatureFlipping on method:{} class:{}", method.getName(), method.getDeclaringClass().getName());
						// invoke same method (interface) with another spring bean (ff.alterBean())
						return method.invoke(appCtx.getBean(ff.alterBean(), method.getDeclaringClass()), pMInvoc.getArguments());
					}
				} else if (ff.alterClazz() != null) {
					Object alterClassBean = findAlterClassBean(pMInvoc, ff.alterClazz());
					if(alterClassBean != null){
						logger.debug("FeatureFlipping on method:{} class:{}", method.getName(), method.getDeclaringClass().getName());
						// invoke same method (interface) with another simple class (ff.alterClass():must have a default constructor
						return method.invoke(alterClassBean, pMInvoc.getArguments());
					}
				} else {
					throw new IllegalArgumentException("FeatFlip: 'alterBeanName' or 'alterClazz' should be provided in " +
									method.getDeclaringClass());
				}
			}
		}
		// No flip, default method invocation
		return pMInvoc.proceed();
	}

	private boolean shouldCallAlterBeanMethod(final MethodInvocation pMInvoc, String alterBean, Logger logger) {
		boolean callAlterBeanMethod = false;
		Method method = pMInvoc.getMethod();
		Component component = pMInvoc.getThis().getClass().getAnnotation(Component.class);
		String currentBeanName = component.value();

		if (alterBean.equals(currentBeanName)) {
			logger.debug("FeatureFlipping on method:{} class:{} already on the alterBean {}", method.getName(), method.getDeclaringClass().getName(), alterBean);
		} else {
			if (!appCtx.containsBean(alterBean)) {
				throw new BeanCreationException("ff4j-aop : bean name '" + alterBean 
						+ "' has not been found in applicationContext still declared in 'alterBean' property of bean " + method.getDeclaringClass());
			}
			callAlterBeanMethod = true;
		}
		return callAlterBeanMethod;
	}

	private Object findAlterClassBean(final MethodInvocation pMInvoc, Class<?> alterClass) throws Exception {
		Method method = pMInvoc.getMethod();
		Class<?> currentClass = pMInvoc.getThis().getClass();
		if (currentClass.equals(alterClass)) {
			logger.debug("FeatureFlipping on method:{} class:{} already on the alterClazz {}", method.getName(), method.getDeclaringClass().getName(), alterClass);
		} else {
			Map<String, ?> beans = appCtx.getBeansOfType(method.getDeclaringClass());
			for (Object bean : beans.values()) {
				if (isBeanAProxyOfAlterClass(bean, alterClass)) {
					return bean;
				}
			}
			throw new BeanCreationException("ff4j-aop : bean with class '" + alterClass 
					+ "' has not been found in applicationContext still declared in 'alterClazz' property of bean " + method.getDeclaringClass());
		}
		return null;
	}
	
	protected boolean isBeanAProxyOfAlterClass(Object proxy, Class<?> alterClass) throws Exception {
		if (AopUtils.isJdkDynamicProxy(proxy)) {
			return ((Advised) proxy).getTargetSource().getTarget().getClass().equals(alterClass);
		} else {
			// expected to be cglib proxy then, which is simply a specialized class
			return proxy.getClass().equals(alterClass);
		}
	}
	
	/** {@inheritDoc} */
	public void setApplicationContext(ApplicationContext applicationContext)
	throws BeansException {
		this.appCtx = applicationContext;
	}
}