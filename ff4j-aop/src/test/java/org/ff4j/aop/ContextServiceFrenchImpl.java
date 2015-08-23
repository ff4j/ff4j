package org.ff4j.aop;

import org.ff4j.core.FlippingExecutionContext;
import org.springframework.stereotype.Component;

@Component("context.french")
public class ContextServiceFrenchImpl implements ContextService {
    @Override
    public String sayHelloWithThreadLocal(String name) {
        return "Bonjour " + name;
    }

    @Override
    public String sayHelloWithParameter(String name, FlippingExecutionContext context) {
        return "Bonjour " + name;
    }
}
