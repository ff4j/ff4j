package org.ff4j.aop;

import java.lang.reflect.Method;
import java.util.HashMap;
import java.util.Map;

import org.springframework.aop.TargetSource;
import org.springframework.aop.framework.autoproxy.AbstractAutoProxyCreator;
import org.springframework.stereotype.Component;

/**
 * This bean will automatically process every in applicationContext.
 * 
 * @author <a href="mailto:cedrick.lunven@gmail.com">Cedrick LUNVEN</a>
 */
@Component("ff.autoproxy")
public class FeatureAutoProxy extends AbstractAutoProxyCreator {

    /** serial number. */
    private static final long serialVersionUID = -364406999854610869L;

    /** Processed Interfaces. */
    private final Map<String, Boolean> processedInterface = new HashMap<String, Boolean>();

    public FeatureAutoProxy() {
        setInterceptorNames(new String[] {FeatureAdvisor.class.getAnnotation(Component.class).value()});
    }

    /** {@inheritDoc} */
    @Override
    protected Object[] getAdvicesAndAdvisorsForBean(Class<?> beanClass, String beanName, TargetSource targetSource) {
        // Scan interface only once.
        if (!beanClass.isInterface() && beanClass.getInterfaces() != null) {
            // Get Interface
            for (Class<?> currentInterface : beanClass.getInterfaces()) {
                Object[] r = addAnnotedInterface(currentInterface);
                if (r != null) {
                    return r;
                }
            }
        }
        return DO_NOT_PROXY;
    }

    private Object[] addAnnotedInterface(Class<?> currentInterface) {
        String currentInterfaceName = currentInterface.getCanonicalName();
        if (!currentInterfaceName.startsWith("java.")) {
            Boolean isInterfaceFlipped = processedInterface.get(currentInterfaceName);
            if (isInterfaceFlipped != null) {
                if (isInterfaceFlipped) {
                    return PROXY_WITHOUT_ADDITIONAL_INTERCEPTORS;
                }
            } else {
                for (Method method : currentInterface.getDeclaredMethods()) {
                    if (method.isAnnotationPresent(Flip.class)) {
                        processedInterface.put(currentInterfaceName, true);
                        return PROXY_WITHOUT_ADDITIONAL_INTERCEPTORS;
                    }
                }
                processedInterface.put(currentInterfaceName, false);
            }
        }
        return null;
    }
}